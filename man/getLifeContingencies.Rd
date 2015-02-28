\name{getLifecontingencyPv}
\alias{getLifecontingencyPv}
\alias{getLifecontingencyPvXyz}
\title{
Functions to obtain the present value of a life contingency given the time to death
}
\description{
It returns the present value of a life contingency, specified by its APV symbol, 
known the time to death ob the sibjects
}
\usage{
getLifecontingencyPv(deathsTimeX, lifecontingency, object, x, t, i = object@interest, 
m = 0, k = 1, payment = "advance")
getLifecontingencyPvXyz(deathsTimeXyz, lifecontingency, tablesList, x, t, i, m = 0, 
k = 1, status = "joint", payment = "advance")
}
\arguments{
  \item{deathsTimeX}{Time to death}
  \item{lifecontingency}{lifecontingency symbol}
  \item{object}{life table(s)}
  \item{x}{age(s) of the policyholder(s)}
  \item{t}{term of the contract}
  \item{i}{interest rate}
  \item{m}{deferrement}
  \item{k}{fractional payments}
  \item{payment}{either "advance" or "deferred"}
  \item{deathsTimeXyz}{matrix of death times from birth}
  \item{tablesList}{list of table of the same size of num column of deathTimeXyz.}
  \item{status}{"joint" or "last" survivor.}
}
\details{This function is a wrapper to the many internal functions that give the PV
 known the age of death.}
\value{A vector or matrix of size number of rows of deathTimeXyz / deathTimeXy}
\references{Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., 
Hickman, J.C., Jones, D.A. and Nesbitt, C.J.}
\author{Spedicato Giorgio}
\note{Multiple life function needs to be tested}
\section{Warning}{The function is provided as is, without any warranty 
regarding the accuracy of calculations. The author disclaims any liability for eventual
	losses arising from direct or indirect use of this software.}
\seealso{
\code{\link{rLifeContingenciesXyz}}, \code{\link{rLifeContingencies}} 
}
\examples{
#simulate the PV values for some life contingencies given some death times
data(soa08Act)
testgetLifecontingencyPvXyzAxyz<-getLifecontingencyPvXyz(deathsTimeXyz=
matrix(c(50,50,51,43,44,22,12,56,20,24,53,12),
ncol=2),
lifecontingency = "Axyz",tablesList = list(soa08Act, soa08Act), i = 0.03, t=30,x=c(40,50),
m=0, k=1,status="last")
testgetLifecontingencyPvAxn<-getLifecontingencyPv(deathsTimeX = seq(0, 110, by=1), 
lifecontingency = "Axn", object=soa08Act, 
		x=40,t=20, m=0, k=1)
}

