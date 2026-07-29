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
###         multiple decrement
###

.MDT_DEFAULT_NAME <- "sample multiple decrement table"
.MDT_DEFAULT_TABLE <- data.frame(
	x = seq(0, 2, 1),
	lx = c(1000, 500, 200),
	c1 = c(200, 200, 100),
	c2 = c(300, 100, 100)
)
# Backward completion uses a fixed survival ratio for synthetic rows.
.MDT_BOTTOM_COMPLETION_SURVIVAL <- 0.99

#defines the multiple decrement class
#at least three or more slots, x, lx and the causes...
setClass("mdt",
		slots = c(name = "character", table = "data.frame")
)
#defines the validity of a multiple decrement table
setValidity("mdt",
		function(object){
			# Collect all validation failures to ease debugging in validObject().
			check <- character(0)
			namesOfTable <- names(object@table)
			if(!all(c("x", "lx") %in% namesOfTable)) {
				check <- c(check, "Missing x or lx")
			}

			if(all(c("x", "lx") %in% namesOfTable)) {
				# Check that x is a consecutive sequence from 0 to max(x).
				if(!setequal(object@table$x, seq(0, max(object@table$x), by = 1))) {
					check <- c(check, "Check the x sequence")
				}

				# drop = FALSE preserves matrix/data.frame semantics with one decrement.
				# Check that the total decrements are equal to initial lx.
				onlyDecrements <- object@table[, setdiff(namesOfTable, c("x", "lx")), drop = FALSE]
				if(sum(onlyDecrements) != object@table$lx[1]) {
					check <- c(check, "Check the lx")
				}
			}

			if(length(check) == 0) {
				TRUE
			} else {
				check
			}
		}
)

#initialize method

setMethod("initialize",
		signature(.Object = "mdt"),
		function (.Object,
		          name = .MDT_DEFAULT_NAME,
		          table = .MDT_DEFAULT_TABLE,
		          ...) 
		{
			table<-.tableSanitizer(decrementDf=table)
			callNextMethod(.Object=.Object, name =name, table=table,...)
		}
)



#method to return type of decrements available
setGeneric("getDecrements", function(object) standardGeneric("getDecrements"))
setMethod("getDecrements","mdt", 
		function(object) {
out<-setdiff(names(object@table),c("x","lx"))
			return(out)
		}
)

#function to check if a number is a probability

.isProb<-function(prob)
{
	if((prob > 1) || (prob < 0)) return(FALSE) else return(TRUE)
}

#tento caricare la tavola
.tableSanitizer<-function(decrementDf)
{
	# Standardize partially specified decrement tables into a full internal layout.
	out<-decrementDf
	namesOfTable<-names(decrementDf)
	decrementIds<-which(!(namesOfTable %in% c("lx","x")))
	pureDecrements<-decrementDf[,decrementIds, drop = FALSE]
	#add the lx columnd
	if(!("lx" %in% namesOfTable))
	{
		lx<-numeric(nrow(decrementDf))
		lx[1]<-sum(pureDecrements)
		for(i in 2:length(lx))
		{
			lx[i]=lx[i-1]-sum(pureDecrements[i-1,, drop = FALSE])
			
		}
		out$lx<-lx
		decrementDf<-out
		cat("Added lx","\n")
	}
	#add the x column
	if(!("x" %in% namesOfTable)) #check if missing x
	{
		x=seq(from=0,to=(nrow(decrementDf)-1),by=1)
		out$x<-x
		decrementDf<-out
		cat("Added x to the table...","\n")
	}
	#complete the table from bottom
	if(!(min(decrementDf$x)==0))
	{
		x2Complete<-seq(from=0,to=(min(decrementDf$x)-1))
		lx2Complete<-numeric(length(x2Complete))
		lxLast<-decrementDf$lx[1]
		for(i in rev(seq_along(lx2Complete)))
		{
			# Reconstruct previous lx values by applying the fixed synthetic survival.
			lx2Complete[i]<-lxLast/.MDT_BOTTOM_COMPLETION_SURVIVAL
			lxLast<-lx2Complete[i]
		}
		dx2Add<- -diff(c(lx2Complete,decrementDf$lx[1]))
		decrements2complete<-matrix(0,nrow=length(dx2Add),ncol=ncol(decrementDf),
				dimnames=list(NULL,c("x","lx",colnames(decrementDf)[decrementIds])))
		decrements2complete[,1]<-x2Complete #writing x
		decrements2complete[,2]<-lx2Complete #writing lx
		decrements2complete[,3]<-dx2Add #writing on the first decrement
		outMatrix<-rbind(decrements2complete,as.matrix(decrementDf))
		out<-as.data.frame(outMatrix)
		rownames(out)<-NULL
		cat("Added fictional decrement below last x and completed x and lx until zero....","\n")
	}
	#complete the table for top
	maxage<-which(out$x==max(out$x))
	pureDecrements<-out[,decrementIds, drop = FALSE]
	lastCheck<-(rowSums(pureDecrements[maxage,, drop = FALSE])==out$lx[maxage])
	if (!lastCheck) {
		# Add one terminal row so that all remaining lives are decremented.
		decrements2complete<-matrix(0,nrow=1,ncol=ncol(decrementDf),dimnames=list(NULL,c("x","lx",colnames(decrementDf)[decrementIds])))
		decrements2complete[1,1]<-max(out$x)+1
		decrements2complete[1,2]<-out$lx[1]-sum(out[,decrementIds, drop = FALSE])
		decrements2complete[1,3]<-decrements2complete[1,2]
		outMatrix<-rbind(out,decrements2complete)
		out<-as.data.frame(outMatrix)
		rownames(out)<-NULL
		cat("Completed the table at top, all decrements on first cause","\n")
	}
	invisible(out)
}

#function to shows probabilities instead of decrements
.decr2Probs<-function(decrementDf)
{
	namesOfTable<-names(decrementDf)
	decrementIds<-which(!(namesOfTable %in% c("lx","x")))
	pureDecrements<-decrementDf[,decrementIds, drop = FALSE]
	probs<-pureDecrements/decrementDf$lx
	rownames(probs)<-decrementDf$x
	invisible(probs)
}

#show method

setMethod("show","mdt", #metodo show
		function(object){
			cat(paste("Multiple decrements table",object@name),"\n")
			object@table
			print(object@table)
		}
)

setMethod("print","mdt", #metodo show
		function(x){
			cat(paste("Multiple decrements table",x@name),"\n")
			probs<-.decr2Probs(x@table)
			print(probs)
		}
)

#export method

#saves mdt as a data frame
setAs("mdt","data.frame",
		function(from){
			return(from@table)
		}
)


#summary method

setMethod("summary",
		signature(object="mdt"),
		function (object, ...)
		{
			cat("This is Multiple Decrements Table: ",object@name, "\n","Omega age is: ",getOmega(object), "\n", "Stored decrements are: ", getDecrements(object))
		}
)