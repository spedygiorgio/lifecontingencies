# TODO: Fix t=0
# 
# Author: Giorgio Alfredo Spedicato
###############################################################################

#decrement specific function

.dxt.mdt<-function(object=object, x=x, t=t, decrement=decrement ) {
	out<-0
	if (t==0) return(0) #no decrement after just 0 seconds
	if(missing(decrement)) {
		decrement.cols<-which(!(names(object@table) %in% c("lx","x")))
	} else {
		if (class(decrement)=="numeric") decrement<-getDecrements(object)[decrement]
		decrement.cols<-which(names(object@table)==decrement)
	}
	#have a check!!!
		ages2consider<-x+0:(t-1)
		age.rows<-which(object@table$x %in% ages2consider)
		out<-sum(object@table[age.rows,decrement.cols])	
	invisible(out)
}

.qxt.mdt<-function(object,x,t,decrement) {
	out<-0
	ageIndex<-which(object@table$x==x)
	lx<-object@table$lx[ageIndex]
	dx<-ifelse(missing(decrement),.dxt.mdt(object=object,x=x, t=t),.dxt.mdt(object=object,x=x, t=t,decrement=decrement))
	out<-dx/lx
	invisible(out)
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
  
  if (class(object) != "mdt") stop("Error! Needed Mdt")
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

