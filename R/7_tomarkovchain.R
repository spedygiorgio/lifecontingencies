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

.lifetable_to_markovchain_list <- function(from) {
	.require_markovchain(".lifetable_to_markovchain_list")

	outChains <- list()
	ages <- seq(0, getOmega(from), 1)
	# qxt() already accepts a vector of ages: compute all qx values with a
	# single call instead of one redundant call per age inside the loop.
	qxVec <- qxt(from, ages, 1)
	for (i in seq_along(ages)) {
		ageMc <- .qxToMc(qx = qxVec[i], age = as.character(ages[i]))
		outChains[[length(outChains) + 1]] <- ageMc
	}
	out <- new("markovchainList", markovchains = outChains, name = from@name)
	invisible(out)
}

.qxToMc<-function(qx, age)
{
	.require_markovchain(".qxToMc")
	statesNames=c("alive","death")
	matr=matrix(rep(0,4),nrow = 2);dimnames(matr) <- list(statesNames,statesNames)
	matr[1,1]=1-qx
	matr[1,2]=qx
	matr[2,1]=0
	matr[2,2]=1
	outMc<-new("markovchain",transitionMatrix=matr,name=age)
	invisible(outMc)
}

#function to convert a mdt to a markovchain list

.mdt_to_markovchain_list <- function(from) {
	.require_markovchain(".mdt_to_markovchain_list")

	outChains <- list()
	ages <- seq(0, getOmega(from), 1)
	pureDecrements <- from@table[, getDecrements(from)]

	for (i in ages) {
		qx <- pureDecrements[i + 1, ] / from@table$lx[i + 1]
		names(qx) <- getDecrements(from)
		ageMc <- .qxdToMc(qx = qx, age = as.character(i))
		outChains[[length(outChains) + 1]] <- ageMc
	}
	out <- new("markovchainList", markovchains = outChains, name = from@name)
	invisible(out)
}

.qxdToMc<-function(qx,age)
{
	.require_markovchain(".qxdToMc")
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

# S4 coercions to "markovchainList". Registered by .onLoad() only when the
# optional markovchain package is installed at load time, so that without it
# as(x, "markovchainList") fails with R's standard "no method or default"
# error and the vignettes skip the chunk (see the vignette's eval option).
.registerMarkovchainCoercions <- function() {
	methods::setAs("lifetable", "markovchainList",
		function(from) invisible(.lifetable_to_markovchain_list(from)))
	methods::setAs("mdt", "markovchainList",
		function(from) invisible(.mdt_to_markovchain_list(from)))
	invisible(TRUE)
}
