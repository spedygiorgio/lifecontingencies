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


# TODO: Fix t=0
# 

#decrement specific function

.dxt.mdt<-function(object=object, x=x, time=time, decrement=decrement ) {
	out<-0
	if (time==0) return(0) #no decrement after just 0 seconds
	if(missing(decrement)) {
		decrement.cols<-which(!(names(object@table) %in% c("lx","x")))
	} else {
		if (is.numeric(decrement)) decrement<-getDecrements(object)[decrement]
		decrement.cols<-which(names(object@table)==decrement)
	}
	#have a check!!!
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
  
  #allocate to df
  
  pxk<-numeric(length(seqk))
  qxkp1<-numeric(length(seqk))
  probs<-numeric(length(seqk))
  
  
  for (j in seqk) {
    pxk[j+1]<-pxt(object=object,x=x,t=j)
    qxkp1[j+1]<-qxt(object=object,x=(x+j),t=1,decrement=decrement)
    probs[j+1]<-pxk[j+1]*qxkp1[j+1]
  }
  out<-presentValue(cashFlows=payments, timeIds=times, interestRates=i, probabilities=probs,power=1)
  return(out)
}

