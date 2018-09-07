#TO DO: check here http://www.mysmu.edu/faculty/yktse/FMA/S_FMA_1.pdf
#TO DO: add k to increasing and decreasing annuities function

#function to evaluate the present value of a series of cash flows
presentValue<-function(cashFlows, timeIds, interestRates, probabilities, power=1)
{
	out<-0
	if(missing(timeIds)) #check coherence on time id vector
	{	warning("Warning: missing time vector")
		timeIds=1
	}
	if(missing(probabilities)) #if no probabilities given than prob=1
	{
		probabilities <- rep(1,length(cashFlows))
	} else {
		if(length(cashFlows)!=length(probabilities)) stop("Error! Probabilities must have same length of cash flows")
	}
	
	if(!(length(cashFlows)==length(timeIds))) stop("Error! check dimensionality of cash flow and time ids vectors") #check dimensionality of cash flows
	if((length(interestRates)>1)&(length(interestRates)!=length(timeIds))) warning("Interest rates incoherent with time ids") #check dimensioanlity of time ids
	
	interestRates <- rep(interestRates,length.out=length(timeIds))
	v <- (1+interestRates)^(-timeIds)
	#power used for APV, usually=1
	out <- sum( ( (cashFlows^power) * (v^power) ) * probabilities) 
  #using Rcpp code seems inefficient
# 	out<-switch(calculation,
#               R=sum(((cashFlows^power)*(v^power))*probabilities),
#               Rcpp=.mult3sum(x=(cashFlows^power),y=(v^power),z=probabilities))
	return(out)
}



#duration
#m=tasso di interesse nominale capitalizzato m volte
duration=function(cashFlows, timeIds,i, k=1,macaulay=TRUE)
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
#' @param k The original / target compounting frequency.
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
#' a force of interest of 0.02 corresponds to an APR of 
#' intensity2Interest(intensity=0.02)
#' @export
intensity2Interest=function(intensity)
{
	out=exp(intensity*1)-1
	return(out)
}
#' @rdname intensity-interest
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