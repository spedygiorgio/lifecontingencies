# TODO: Add comment
# 
# Author: Giorgio Alfredo Spedicato
###############################################################################


#functions to convert a lifetable toward a MarkovChainList

.qxToMc<-function(qx, age)
{
	statesNames=c("alive","death")
	matr=matrix(rep(0,4),nrow = 2);dimnames(matr) <- list(statesNames,statesNames)
	matr[1,1]=1-qx
	matr[1,2]=qx
	matr[2,1]=0
	matr[2,2]=1
	outMc<-new("markovchain",transitionMatrix=matr,name=age)
	invisible(outMc)
}

setAs("lifetable","markovchainList",
		function(from)
		{
			outChains<-list()
			ages<-seq(0,getOmega(from),1)
			for(i in ages)
			{
				ageMc<-.qxToMc(qx=qxt(from,i,1),age=as.character(i))
				outChains[[length(outChains)+1]]<-ageMc
			}
			out<-new("markovchainList",markovchains=outChains,name=from@name)
			invisible(out)
		}
	)

#function to convert a mdt to a markovchain list

.qxdToMc<-function(qx,age)
{
	statesNames=c("alive",names(qx))
	matr<-matrix(0,ncol=length(statesNames), nrow=length(statesNames)) #preallocate matrix
	colnames(matr)<-statesNames
	rownames(matr)<-statesNames
	diag(matr)<-1 #set states other than alive as absorbing
	matr[1,1]<-1-sum(qx)
	for(j in 1:length(qx))  matr[1,j+1]<-as.numeric(qx[j])
	outMc<-new("markovchain",transitionMatrix=matr,name=as.character(age))	
	invisible(outMc)
}

setAs("mdt","markovchainList",
		function(from)
		{
			outChains<-list()
			ages<-seq(0,getOmega(from),1)
			pureDecrements<-from@table[,getDecrements(from)]
			
			for(i in ages)
			{
				qx<-pureDecrements[i+1,]/from@table$lx[i+1]
				names(qx)<-getDecrements(from)
				ageMc<-.qxdToMc(qx=qx,age=as.character(i))
				outChains[[length(outChains)+1]]<-ageMc
			}
			out<-new("markovchainList",markovchains=outChains,name=from@name)
			invisible(out)
		}
)
