# TODO: Add comment
# 
# Author: Packard Bell
###############################################################################

#decrement specific function

.dxt.mdt<-function(object=object, x=x, t=t, decrement=decrement ) {
	out<-0
	
	if(missing(decrement)) {
		decrement.cols<-which(!(names(object@table) %in% c("lx","x")))
	} else {
		if (class(decrement)=="numeric") decrement<-getDecrements(object)[decrement]
		decrement.cols<-which(names(object@table)==decrement)
	}
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