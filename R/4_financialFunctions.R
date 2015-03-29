#TO DO: check here http://www.mysmu.edu/faculty/yktse/FMA/S_FMA_1.pdf
#TO DO: add k to increasing and decreasing annuities function
#function to evaluate the present value of a series of cash flows

presentValue<-function(cashFlows, timeIds,interestRates, probabilities, power=1)
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
	v <- (1+interestRates)^-timeIds
	out <- sum(((cashFlows^power)*(v^power))*probabilities) #power used for APV, usually=1
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
	
	interestRates=rep(i/k,length.out=length(timeIds))
	#calcola il valora attuale
	ts=timeIds*k
	v=(1+interestRates)^-(ts)
	pv=sum((cashFlows*v))
	#pv=.C("add2", x=as.double(cashFlows), y=as.double(v),n=as.integer(length(cashFlows)),out=numeric(1))$out
	#calcola il tempo medio ponderato
	weightedTime=sum((cashFlows*v*ts))
	#weightedTime=.C("add3", x=as.double(cashFlows), y=as.double(v),z=as.double(ts),n=as.integer(length(cashFlows)),out=numeric(1))$out
	out=weightedTime/pv	
	if(macaulay==FALSE) out=out else out=out/(1+i/k) 
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
	out=(n-annuity(i=i, n=n, type="immediate"))/n
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
#	if(type=="immediate") timeIds=seq(from=1, to=n, by=1)
#	else timeIds=seq(from=0, to=n-1, by=1) #due
#	timeIds=-timeIds
#	iRate=rep(i,length.out=n)
#	out=presentValue(cashFlows=rep(1,length.out=n),i = iRate, timeIds=timeIds)
	out=(1+i)^n*annuity(i=i,n=n,k=k,m=m,type=type)
	return(out)
}
#obtain the nominal interest rate
nominal2Real=function(i, k=1, type="interest")
{
	out=NULL
	if(type=="interest") out=(1+i/k)^k-1 else 
		out=1-(1-i/k)^k
	return(out)
}
#or
convertible2Effective=function(i, k=1, type="interest")
{
	return(nominal2Real(i=i,k=k,type=type))
}
#obtain the real interest rate
real2Nominal=function(i, k=1, type="interest")
{
	if(type=="interest") out=((1+i)^(1/k)-1)*k else
		out=k*(1-(1-i)^(1/k))
	return(out)
}
#or
effective2Convertible=function(i, k=1, type="interest")
{
	return(real2Nominal(i=i,k=k,type=type))
}

#obtain the interest from intensity
intensity2Interest=function(intensity)
{
	out=exp(intensity*1)-1
	return(out)
}
#obtain the intensity rate from the interest rate
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