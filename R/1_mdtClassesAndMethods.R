# TODO: Add comment
# 
# Author: Giorgio Spedicato
###############################################################################

#defines the multiple decrement class
#at least three or more slots, x, lx and the causes...
setClass("mdt",
		representation(name="character",table="data.frame"),
		prototype(name="sample multiple decrement table",table=data.frame(x=seq(0,2,1),
						lx=c(1000,500,200),c1=c(200,200,100),c2=c(300,100,100)))
)
#defines the validity of a multiple decrement table
setValidity("mdt",
		function(object){
			#verify it contains x and lx
			check<-FALSE
			namesOfTable<-names(object@table)
			check1<-is.element("x",namesOfTable) & is.element("lx",namesOfTable)
			if (!check1) cat("Missing x or lx")
			#cat("check1",check1,"\n")
			#check that x is a sequence from 0 to max 
			#check2<-setequal(object@table$x,seq(min(object@table$x),max(object@table$x),by=1))
			check2<-setequal(object@table$x,seq(0,max(object@table$x),by=1))
			if (!check2) cat("Check the x sequence")
			#cat("check2",check2,"\n")
			#check the sum of the dx to be equal to lx0
			onlyDecrements<-object@table[,setdiff(namesOfTable,c("x","lx"))]
			check3<-(sum(onlyDecrements)==object@table$lx[1])
			#cat(object@table$lx[1],"\n")
			#cat(sum(onlyDecrements),"\n")
			if (!check3) cat("Check the lx")
			#cat("check3",check3,"\n")
			check<-check1&check2&check3
			return(check)
		}
)

#initialize method

setMethod("initialize",
		signature(.Object = "mdt"),
		function (.Object, name,table,...) 
		{
			if(missing(table)) table=data.frame(x=seq(0,2,1),
						lx=c(1000,500,200),c1=c(200,200,100),c2=c(300,100,100))	
			if(missing(name)) name="sample multiple decrement table"
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
  if((prob>1)|(prob<0)) return(FALSE) else return(TRUE)
}

#tento caricare la tavola
.tableSanitizer<-function(decrementDf)
{
	out<-decrementDf
	namesOfTable<-names(decrementDf)
	decrementIds<-which(!(namesOfTable %in% c("lx","x")))
	pureDecrements<-decrementDf[,decrementIds]
	#add the lx columnd
	if(!("lx" %in% namesOfTable))
	{
		lx<-numeric(nrow(decrementDf))
		lx[1]<-sum(pureDecrements)
		for(i in 2:length(lx))
		{
			lx[i]=lx[i-1]-sum(pureDecrements[i-1,])
			
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
		for(i in length(lx2Complete):1)
		{
			lx2Complete[i]<-lxLast/(1-0.01)
			lxLast<-lx2Complete[i]
		}
		dx2Add<- -diff(c(lx2Complete,decrementDf$lx[1]))
		decrements2complete<-matrix(0,nrow=length(dx2Add),ncol=ncol(decrementDf),
				dimnames=list(NULL,c("x","lx",names(decrementDf[,decrementIds]))))
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
	pureDecrements<-out[,decrementIds]
	lastCheck<-(rowSums(pureDecrements[maxage,])==out$lx[maxage])
	if (!lastCheck) {
		decrements2complete<-matrix(0,nrow=1,ncol=ncol(decrementDf),dimnames=list(NULL,c("x","lx",names(decrementDf[,decrementIds]))))
		decrements2complete[1,1]<-max(out$x)+1
		decrements2complete[1,2]<-out$lx[1]-sum(out[,decrementIds])
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
	pureDecrements<-decrementDf[,decrementIds]
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