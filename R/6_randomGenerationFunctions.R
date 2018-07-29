# TODO: function to generate Iaxn variates
# TODO: check axn
# 
# Author: Giorgio A. Spedicato
###############################################################################




##########random variables Tx and Kx generators 

#
#n=100
#object=soa08Act
#x=20
#k=2
#type="Tx"
#modified dxt to return the 1/k fraction of deaths between
#x and t

.dxtk<-function(object, x,t,k){
out=NULL
out=ifelse((x>getOmega(object)),0,dxt(object = object, 
		x=floor(x),t=t)/k)
	return(out)
}



#' @name rLifes
#' @aliases rLifexyz
#' @title Function to generate random future lifetimes
#'
#' @param n Number of variates to generate
#' @param object An object of class lifetable
#' @param x The attained age of subject x, default value is 0
#' @param k Number of periods within the year when it is possible death to happen, default value is 1
#' @param type Either \code{"Tx"} for continuous future lifetime, \code{"Kx"} for curtate furture lifetime (can be abbreviated).
#' 
#' @details Following relation holds for the future life time: \eqn{T_x=K_x+0.5}
#' @references 	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
#' @note The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 	losses arising from direct or indirect use of this software.
#' @seealso \code{\linkS4class{lifetable}}, \code{\link{exn}}
#'
#' @return A numeric vector of n elements.
#'
#' @examples
#' \dontrun{
#' ##get 20000 random future lifetimes for the Soa life table at birth
#' data(soa08Act)
#' lifes=rLife(n=20000,object=soa08Act, x=0, type="Tx")
#' check if the expected life at birth derived from the life table is statistically equal 
#' to the expected value of the sample
#' t.test(x=lifes, mu=exn(soa08Act, x=0, type="continuous"))
#' }
#' @export
rLife<-function(n,object, x=0,k=1, type="Tx")
{
	if(missing(n)) stop("Error! Needing the n number of variates to return")
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(x>getOmega(object)) stop("Error! x > maximum attainable age")
  type <- testtypelifearg(type)
  
	out=numeric(n)
	const2Add=0
	if(type=="Tx") const2Add=0.5/k # the continuous future lifetime 
	#is the curtate future lifetime + 0.5
	#determine the deaths
	omega=getOmega(object)
	#the sequence of possible death periods 
	#sequence=seq(from=0, to=omega, by=1)
	sequence=seq(from=0, to=omega+1, by=1/k)
	#dx<-sapply(sequence,dxt,object=object,t=1)
	dx<-sapply(sequence,.dxtk,object=object,t=1,k=k)
	#determine the perimeter of x
#	index=which(object@x>=x)
#	x2Sample<-object@x[index]
	index=which(sequence>=x)
	x2Sample<-sequence[index]
	deathsOfSample<-dx[index]
	probsOfDeath<-deathsOfSample/sum(deathsOfSample)	
	out=sample(x=x2Sample,size=n,replace=TRUE, prob=probsOfDeath)+const2Add-x
	return(out)
}

#ciao<-rLife(100000,soa08Act,x=25,type="Kx")
#t.test(x=ciao, mu=exn(soa08Act,25))


#' @rdname rLifes
#'
#' @param tablesList An list of lifetables
#' @examples
#' \dontrun{
#' #assessment of curtate expectation of future lifetime of the joint-life status
#' #generate a sample of lifes
#' data(soaLt)
#' soa08Act=with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
#' tables=list(males=soa08Act, females=soa08Act)
#' xVec=c(60,65)
#' test=rLifexyz(n=50000, tablesList = tables,x=xVec,type="Kx")
#' #check first survival status
#' t.test(x=apply(test,1,"min"),mu=exyzt(tablesList=tables, x=xVec,status="joint"))
#' #check last survival status
#' t.test(x=apply(test,1,"max"),mu=exyzt(tablesList=tables, x=xVec,status="last"))
#' }
#' @export
rLifexyz <- function(n,tablesList,x,k=1, type="Tx")
{
	#initial checkings
	numTables=length(tablesList)
	if(length(x)!=numTables) stop("Error! Initial ages vector length does not match with number of lives")
	for(i in 1:numTables) {
		if(!(class(tablesList[[i]]) %in% c("lifetable", "actuarialtable"))) stop("Error! A list of lifetable objects is required")
	}	
	type <- testtypelifearg(type)
	
	outVec <- numeric(0)
	for(i in 1:numTables)
	{
		outI <- numeric(n)
		outI <- rLife(n=n,object=tablesList[[i]],x=x[i],k=k,type=type)
		outVec <- c(outVec,outI)
	}	
	out<- matrix(data=outVec,nrow=n,ncol=numTables,byrow=FALSE)
	return(out)
}


##get 20000 random future lifetimes for the Soa life table at birth
#data(soa08Act)
#lifes=rLife(n=1000000,object=soa08Act, x=0, type="Tx")
###check if the expected life at birth derived from the life tableis statistically equal to the expected value of the sample
##
#t.test(x=lifes, mu=exn(soa08Act, x=0, type="continuous"))
#
#lifes=rLife(n=2000000,object=soa08Act, x=0, type="Tx",k=12)
#t.test(x=lifes, mu=exn(soa08Act, x=0, type="continuous"))
#
#lifes=rLife(n=500000,object=soa08Act, x=50, type="Tx",k=12)
#t.test(x=lifes, mu=exn(soa08Act, x=50, type="continuous"))

#lifes=rLife(n=500000,object=soa08Act, x=50, type="Kx",k=1)
#t.test(x=lifes, mu=exn(soa08Act, x=50, type="curtate"))


#y=absolute policyholder's age
#T=death period
#n=length of insurance
#m=deferring period
#k=payments' frequency


#pure endowment function
.fExn<-function(T,y,n, i)
{
	out=ifelse(T<y+n,0,(1+i)^(-n))
	return(out)
}


#
# x=40
# t=40
# n=50000
# object=soa08Act
# lifecontingency="Exn"
# i=0.06
#
# outs<-rLifeContingencies(n=n,lifecontingency=lifecontingency, object=object, x=x,t=t,i=i, 
#		m=m,k=k, parallel=TRUE)
#APV=Exn(object, x=x, n=t)
#t.test(x=outs, mu=APV)

#term life insurance function
#seems to work
.fAxn<-function(T,y,n, i, m, k)
{
	out=numeric(1)
	out=ifelse(((T>=y+m) && (T<=y+m+n-1/k)),(1+i)^-(T-y+1/k),0)
	return(out)
}


# x=40
# t=100
# n=50000
# object=soa08Act
# lifecontingency="axn"
# i=0.06
# m=30
# k=12
##
# outs<-rLifeContingencies(n=n,lifecontingency=lifecontingency, object=object, x=x,t=t,i=i, 
#		m=m,k=k, parallel=TRUE)
#APV=Axn(object, x=x, k=k,m=m,i=i)
# mean(outs)
## APV
# t.test(x=outs, mu=APV)


#multiple heads term life insurance function
#seems to work
.fAxyzn<-function(T,y,n, i, m, k, status)
{
  status <- teststatusarg(status)
	out=numeric(1)
	temp=T-y
	T=ifelse(status=="joint",min(temp),max(temp)) #redefines T
	out=ifelse(((T>=m) && (T<=m+n-1/k)),(1+i)^-(T+1/k),0)
	return(out)
}


#increasing life insurance function

.fIAxn<-function(T,y,n, i, m, k=1)
{
	out=numeric(1)
	#if policyholder dies in the insured period (y + m  --- y+m+n -1/k
	out=ifelse(((T>=y+m) && (T<=y+m+n-1/k)),(T-(y+m)+1/k)*(1+i)^-(T-y+1/k),0)
	return(out)
}


#decreasing life insurance function

.fDAxn<-function(T,y,n, i, m, k=1)
{
	out=numeric(1)
	out=ifelse(((T>=y+m) && (T<=y+m+n-1/k)),(n-(T-(y+m)+1/k))*(1+i)^-(T-y+1/k),0)
	return(out)
}


#annuity example: DOES NOT WORK
# y = actual age
# Age of death
# n number of payments

#
#lifecontingency="axn"
#object=soa08Act
#x=65
#m=0
#n=10
#k=12
#
#APV=axn(soa08Act, x=x,m=m,n=n,k=k)
#ciao=rLifeContingencies(n=25000,lifecontingency = lifecontingency,object=object,m=m,t=n,x=x,k=k,parallel=TRUE)
#
#t.test(mu=APV, x=ciao)
#mean(ciao)
#APV
#



.faxn<-function(T,y,n, i, m, k=1, payment="advance")
{
	out=numeric(1)
	payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
	K=T-y #number of years to live
		if(K<m) { #if policyholder dies before inception of the annuity
			out=0 #no payment is due
		} else {
		  times=seq(from=m, to=min(m+n-1/k,K),by=1/k) #else it pays from m to the min of m + n - 1/k
      if (payment=="immediate") times = times + 1/k;
 		  out=presentValue(cashFlows=rep(1/k, length(times)), timeIds=times, interestRates=i)
		}
	return(out)
}

#for multiple life contingencies
#
#status="last"
#x=c(20,20)
#tablesList=list(soa08Act,soa08Act)
#k=1
#n=10
#m=0
#
#APV=axyzn(tablesList,x=x,n=n,m=m,k=k,status=status, type="EV")
#APV
#ciao=rLifeContingenciesXyz(n=25000,lifecontingency = "axyz",tablesList=tablesList, x=x,t=n,m=m,k=k,status=status,parallel=TRUE)
#mean(ciao)
#t.test(mu=APV, x=ciao)


#it does not work


.faxyzn<-function(T,y,n, i, m, k=1,status, payment="advance")
{
	out=numeric(1)
	payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
	status <- teststatusarg(status)
	
	temp=T-y
	K=ifelse(status=="joint",min(temp),max(temp))
	if(K<m) { #if policyholder dies before inception of the annuity
				out=0 #no payment is due
			} else {
				times=seq(from=m, to=min(m+n-1/k,K),by=1/k) #else it pays from m to the min of m + n - 1/k
				if (payment=="immediate") times = times + 1/k #copy from univariate
				out=presentValue(cashFlows=rep(1/k, length(times)), timeIds=times, interestRates=i)
			}	
	return(out)
}


#to be fixed!!!
.fIaxn<-function(T,y,n, i, m, k=1)
{
	out=numeric(1)
	K=T-y 
	numOfPayments=max(min(n,K+1-m),0)
	out=increasingAnnuity(i=i,n=numOfPayments,type="immediate")
	return(out)
}

#n-year endowment insurance function
.fAExn<-function(T,y,n, i, k)
{
	out=numeric(1)
	out=ifelse(((T>=y) && (T<=y+n-1/k)),(1+i)^-(T-y+1/k),(1+i)^-n)
	return(out)
}



#x=40
#t=10
#n=5000
#object=soa08Act
#lifecontingency="Axn"
#i=0.06
#m=0
#k=1


rLifeContingencies<-function (n, lifecontingency, object, x, t, i = object@interest, 
		m = 0, k = 1, parallel = FALSE, payment="advance") 
{
  payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
  lifecontingency <- testlifecontarg(lifecontingency)
  
	deathsTimeX = numeric(n)
	outs = numeric(n)
	if (k == 1) 
		deathsTimeX = x + rLife(n = n, object = object, x = x, 
				k = k, type = "Kx")
	else deathsTimeX = x + rLife(n = n, object = object, x = x, 
				k = k, type = "Tx")
	if (parallel == TRUE) {
		#require(parallel)
		type <- if (exists("mcfork", mode = "function")) 
					"FORK"
				else "PSOCK"
		cores <- getOption("mc.cores", detectCores())
		cl <- makeCluster(cores, type = type)
		clusterExport(cl, varlist = c("presentValue", "annuity"))
		if (lifecontingency == "Axn") 
			outs = parSapply(cl = cl, deathsTimeX, .fAxn, y = x, 
					n = t, i = i, m = m, k = k)
		else if (lifecontingency == "Exn") 
			outs = parSapply(cl = cl, deathsTimeX, .fExn, y = x, 
					n = t, i = i)
		else if (lifecontingency == "IAxn") 
			outs = parSapply(cl = cl, deathsTimeX, .fIAxn, y = x, 
					n = t, i = i, m = m, k = k)
		else if (lifecontingency == "DAxn") 
			outs = parSapply(cl = cl, deathsTimeX, .fDAxn, y = x, 
					n = t, i = i, m = m, k = k)
		else if (lifecontingency == "AExn") 
			outs = parSapply(cl = cl, deathsTimeX, .fAExn, y = x, 
					n = t, i = i, k = k)
		else if (lifecontingency == "axn") {
			if (missing(t)) 
				t = getOmega(object) - x - m
			outs = parSapply(cl = cl, deathsTimeX, .faxn, y = x, 
					n = t, i = i, m = m, k = k, payment=payment)
		}
		stopCluster(cl)
	}
	#not parallel
	else {
		if (lifecontingency == "Axn") 
			outs = sapply(deathsTimeX, .fAxnCpp, y = x, n = t, i = i, 
					m = m, k = k)
		else if (lifecontingency == "Exn") 
			outs = sapply(deathsTimeX, .fExnCpp, y = x, n = t, i = i)
		else if (lifecontingency == "IAxn") 
			outs = sapply(deathsTimeX, .fIAxnCpp, y = x, n = t, 
					i = i, m = m, k = k)
		else if (lifecontingency == "DAxn") 
			outs = sapply(deathsTimeX, .fDAxnCpp, y = x, n = t, 
					i = i, m = m, k = k)
		else if (lifecontingency == "AExn") 
			outs = sapply(deathsTimeX, .fAExn, y = x, n = t, 
					i = i, k = k)
		else if (lifecontingency == "axn") {
			if (missing(t)) 
				t = getOmega(object) - x - m
			outs = sapply(deathsTimeX, .faxn, y = x, n = t, i = i, 
					m = m, k = k, payment=payment)
		}
	}
	return(outs)
}

# x=50
# t=30
# n=50e3
# object=soa08Act
# lifecontingency="IAxn"
# i=0.06
# m=10
# k=12
# 
# outs<-rLifeContingencies(n=n,lifecontingency=lifecontingency, object=object, x=x,t=t,i=i, 
# 		m=m,k=k, parallel=FALSE)
# APV=IAxn(object, x=x,n=t, k=k,m=m)
# mean(outs)
# APV
# t.test(x=outs, mu=APV)
# # 
#  system.time(rLifeContingencies(n,lifecontingency, object, x,t,i=i, m=m,k=k, parallel=FALSE))
#  system.time(rLifeContingenciesR(n,lifecontingency, object, x,t,i=i, m=m,k=k, parallel=FALSE))


#n=10000
#lifecontingency="Axyz"
#tablesList=list(soa08Act,soa08Act)
#x=c(60,60)
#i=0.06
#m=0
#status="joint"
#t=30
#k=1
#
#APV=Axyzn(tablesList=tablesList,x=x,n=t,m=m,k=k,status=status,type="EV")
#ciao<-rLifeContingenciesXyz(n=n,lifecontingency = lifecontingency,tablesList = tablesList,x=x,t=t,m=m,k=k,status=status, 
#		parallel=TRUE)
#APV
#mean(ciao)

rLifeContingenciesXyz<-function(n,lifecontingency, tablesList, x,t,i, 
		m=0,k=1, status="joint", parallel=FALSE, payment="advance")
{
  payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
  lifecontingency <- testlifecontarg2(lifecontingency)
  status <- teststatusarg(status)
  
	numTables=length(tablesList)
	#gets the missing i
	if(missing(i)) {
		temp=0
		for(j in 1:numTables) temp=temp+tablesList[[j]]@interest
		interest=temp/numTables
	} else interest=i #fix giorgio 14-07-2013
	#gets the missing t
	if(missing(t)) {
		t=0
		omega=numeric(numTables)
		for(j in 1:numTables) omega[j]=getOmega(tablesList[[j]])
		temp=omega-x-m
		#t=min(temp) #see below correction 5th Jan 2014
		t <- ifelse(status=="joint",min(temp),max(temp)) #FIX by Kevin Owens
	} 	

	temp=matrix(nrow=n, ncol=numTables)
	outs=numeric(n)
	#fractional payment are handled using countinuous lifetime simulation
	if(k==1) temp=x+rLifexyz(n=n,tablesList=tablesList,x=x, k=k,type="Kx") 
	else temp=x+rLifexyz(n=n,tablesList=tablesList,x=x,k=k,type="Tx") #this to handle fractional payments (assume continuous...)

	deathsTimeX<-temp	
	if(parallel==TRUE) {
		#set up parallel library
		#require(parallel)
		type <- if (exists("mcfork", mode="function")) "FORK" else "PSOCK"
		cores <- getOption("mc.cores", detectCores())
		cl <- makeCluster(cores, type=type)
		clusterExport(cl, varlist=c("presentValue","annuity")) #richiesto da axn
		if(lifecontingency=="Axyz") 
			outs<-parApply(cl=cl, deathsTimeX, 1,.fAxyzn,y=x,n=t, i=interest,m=m,k=k,status=status)
		else if(lifecontingency=="axyz") 
		{			
			outs<-parApply(cl=cl,deathsTimeX, 1,.faxyzn,y=x,n=t,i=interest,m=m,k=k,status=status,payment=payment)
		}
		#stops the cluster

		stopCluster(cl)
	} else {
		#serial version
		if(lifecontingency=="Axyz") 
			outs<-apply( deathsTimeX, 1, .fAxyzn,y=x,n=t, i=interest,m=m,k=k,status=status)
		else if(lifecontingency=="axyz") 
		{
			outs<-apply(deathsTimeX, 1, .faxyzn,y=x,n=t, i=interest,m=m,k=k,status=status,payment=payment)
		}
	}
	return(outs)
}




#functio to obtain the present value of a life contingency function given that the death time of the 
#subject is deathsTimeX (measured from the birth).

getLifecontingencyPv<-function (deathsTimeX, lifecontingency, object, x, t, i = object@interest, 
		m = 0, k = 1,  payment="advance") 
{
  payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
  
	outs = numeric(length(deathsTimeX))
	if (lifecontingency == "Axn") 
		outs = sapply(deathsTimeX, .fAxn, y = x, n = t, i = i, 
				m = m, k = k)
	else if (lifecontingency == "Exn") 
		outs = sapply(deathsTimeX, .fExn, y = x, n = t, i = i)
	else if (lifecontingency == "IAxn") 
		outs = sapply(deathsTimeX, .fIAxn, y = x, n = t, 
				i = i, m = m, k = k)
	else if (lifecontingency == "DAxn") 
		outs = sapply(deathsTimeX, .fDAxn, y = x, n = t, 
				i = i, m = m, k = k)
	else if (lifecontingency == "AExn") 
		outs = sapply(deathsTimeX, .fAExn, y = x, n = t, 
				i = i, k = k)
	else if (lifecontingency == "axn") {
		if (missing(t)) 
			t = getOmega(object) - x - m
		outs = sapply(deathsTimeX, .faxn, y = x, n = t, i = i, 
				m = m, k = k, payment=payment)
		
	}
	return(outs)
}



getLifecontingencyPvXyz<-function(deathsTimeXyz,lifecontingency, tablesList, x,t,i, 
		m=0,k=1, status="joint", payment="advance")
{
  payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
  status <- teststatusarg(status)
  
	numTables=length(tablesList)
	if(ncol(deathsTimeXyz)!=numTables) stop("Error! deathTimeXyz columns should match the number of life tables!")
	#gets the missing i
	if(missing(i)) {
		temp=0
		for(j in 1:numTables) temp=temp+tablesList[[j]]@interest
		interest=temp/numTables
	} else interest=i
	#gets the missing t
	if(missing(t)) {
		t=0
		omega=numeric(numTables)
		for(j in 1:numTables) omega[j]=getOmega(tablesList[[j]])
		temp=omega-x-m
		t=min(temp)
	} 	
	
#	temp=matrix(nrow=n, ncol=numTables)
	outs=numeric(nrow(deathsTimeXyz))
		#serial version
		if(lifecontingency=="Axyz") 
			outs=sapply( deathsTimeXyz, .fAxyzn,y=x,n=t, i=interest,m=m,k=k,status=status)
		else if(lifecontingency=="axyz") 
		{
			outs=sapply(deathsTimeXyz, .faxyzn,y=x,n=t, i=interest,m=m,k=k,status=status,payment=payment)
		}
	return(outs)
}


#testgetLifecontingencyPvXyzAxyz<-getLifecontingencyPvXyz(deathsTimeXyz=matrix(c(50,50,51,43,44,22,12,56,20,24,53,12), ncol=2),
#		lifecontingency = "Axyz",tablesList = list(soa08Act, soa08Act), i = 0.03, t=30,x=c(40,50),m=0, k=1,status="last")
#

#testgetLifecontingencyPvAxn<-getLifecontingencyPV(deathsTimeX = seq(0, 110, by=1), lifecontingency = "Axn", object=soa08Act, 
#		x=40,t=20, m=0, k=1)
#
#testgetLifecontingencyPvaxn<-getLifecontingencyPV(deathsTimeX = seq(0, 110, by=1), lifecontingency = "axn", object=soa08Act, 
#		x=40,t=20, m=0, k=1)


#' Simulate from a multiple decrement table
#' 
#' @param n Number of simulations.
#' @param object The \code{mdt} object to simulate from.
#' @param x the period to simulate from.
#' @param t the period until to simulate.
#' @param t0 initial status (default is "alive").
#' @param include.t0 should initial status to be included (default is TRUE)?
#' @return A matrix with n columns (the length of simulation) and either t (if initial status 
#' is not included) or t+1 rows.
#' @author Giorgio Spedicato
#' 
#' @section Details:
#' The functin uses \code{rmarkovchain} function from markovchain package to simulate the chain
#' 
#' @seealso \code{\link{rLifeContingenciesXyz}},\code{\link{rLifeContingencies}}
#' 
#' @examples
#' mdtDf<-data.frame(x=c(0,1,2,3),death=c(100,50,30,10),lapse=c(150,20,2,0))
#' myMdt<-new("mdt",name="example Mdt",table=mdtDf)
#' ciao<-rmdt(n=5,object = myMdt,x = 0,t = 4,include.t0=FALSE,t0="alive")
#' 

rmdt<-function(n=1,object, x=0,t=1,t0="alive", include.t0=TRUE) {
	#require(markovchain)
	mcList<-as(object, "markovchainList")
	initialVal<-rep(t0,n)
	endSim<-min(t,getOmega(object)-x)
	row.names<-character()
  
	if (include.t0==TRUE) {
   
    outMatr<-matrix("",nrow=endSim+1, ncol=n)
    row.names<-c(row.names,x)
    outMatr[1,]<-rep(t0,n)
	} else {
	  outMatr<-matrix("",nrow=endSim, ncol=n)
	}
	#define the maximum go - ahead depth
	for(i in 1:endSim)
	{
    simulations<-character(n)
    #the row names is the state at the beginning of period t+1
		row.names <- c(row.names,mcList[[x+i+1]]@name)  
    for(j in 1:n) {
      #this simulate the transition between period t and period t+1
      simJ <- rmarkovchain(n=1,object=mcList[[x+i]],t0=initialVal[j])
      simulations[j] <- simJ
		  
    }
    
		initialVal <- simulations
		if (include.t0==TRUE) outMatr[i+1,]<-simulations else outMatr[i,]<-simulations
	}
  
  rownames(outMatr) <- row.names
  colnames(outMatr) <- 1:n

	return(outMatr)
}


# mdtDf<-data.frame(x=c(0,1,2,3),death=c(100,50,30,10),lapse=c(150,20,2,0))
# 
# myMdt<-new("mdt",name="example Mdt",table=mdtDf)
# 
# ciao<-rmdt(n=10,object = myMdt,x = 0,t = 10,include.t0=FALSE,t0="alive")

#print(ciao)