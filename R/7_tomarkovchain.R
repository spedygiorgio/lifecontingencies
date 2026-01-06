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
###         markov chain related functions
###

# TODO: Add comment
# 
# Author: Giorgio Alfredo Spedicato
###############################################################################


#functions to convert a lifetable toward a MarkovChainList

.qxToMc<-function(qx, age)
{
	if (!.require_markovchain(".qxToMc")) {
		return(invisible(NULL))
	}
	statesNames=c("alive","death")
	matr=matrix(rep(0,4),nrow = 2);dimnames(matr) <- list(statesNames,statesNames)
	matr[1,1]=1-qx
	matr[1,2]=qx
	matr[2,1]=0
	matr[2,2]=1
	outMc<-new("markovchain",transitionMatrix=matr,name=age)
	invisible(outMc)
}

if (requireNamespace("markovchain", quietly = TRUE)) {
	setAs("lifetable","markovchain::markovchainList",
			function(from)
			{
				outChains<-list()
				ages<-seq(0,getOmega(from),1)
				for(i in ages)
				{
					ageMc<-.qxToMc(qx=qxt(from,i,1),age=as.character(i))
					outChains[[length(outChains)+1]]<-ageMc
				}
				out<-new("markovchain::markovchainList",markovchains=outChains,name=from@name)
				invisible(out)
			}
		)
}

#function to convert a mdt to a markovchain list

.qxdToMc<-function(qx,age)
{
	if (!.require_markovchain(".qxdToMc")) {
		return(invisible(NULL))
	}
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

if (requireNamespace("markovchain", quietly = TRUE)) {
	setAs("mdt","markovchain::markovchainList",
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
				out<-new("markovchain::markovchainList",markovchains=outChains,name=from@name)
				invisible(out)
			}
		)
}
