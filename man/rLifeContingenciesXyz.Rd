\name{rLifeContingenciesXyz}
\alias{rLifeContingenciesXyz}
\title{Function to return samples from lifecontingencies on multiple heads}
\description{
This function returns samples from multiple heads life contingent insurances.
}
\usage{
rLifeContingenciesXyz(n, lifecontingency, tablesList, x, t, i, m = 0, 
k = 1, status = "joint", parallel = FALSE,payment = "advance")
}
\arguments{
  \item{n}{Sample size}
  \item{lifecontingency}{Either \code{"Axyz"} or \code{"axyz"}}
  \item{tablesList}{List of tables}
  \item{x}{Ages vector}
  \item{t}{Term}
  \item{i}{Interest rate}
  \item{m}{Deferral period}
  \item{k}{Frequency of payments}
  \item{status}{Either \code{"joint"} for the joint-life status model or \code{"last"}
for the last-survivor status model (can be abbreviated).}
  \item{parallel}{Use parallel computation}
  \item{payment}{
The Payment type, either \code{"advance"} for the annuity due (default)
or \code{"arrears"} for the annuity immediate.
Alternatively, one can use \code{"due"} or \code{"immediate"}
	respectively (can be abbreviated).
}
}
\details{This function should return samples from multiple life contingent insurances.}
\value{A matrix of variates
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{Giorgio Alfredo Spedicato, Kevin J. Owens.}
\note{
A matrix is returned.
}
\section{Warning }{The function is experimental and it its early stages. Samples are biased.} 

\seealso{
\code{\link{rLifeContingencies}},\code{\link{Axyzn}},\code{\link{axyzn}} 
}
\examples{
data(soa08Act)
n=10000
lifecontingency="Axyz"
tablesList=list(soa08Act,soa08Act)
x=c(60,60)
i=0.06
m=0
status="joint"
t=30
k=1
#
APV=Axyzn(tablesList=tablesList,x=x,n=t,m=m,k=k,status=status,type="EV")
samples<-rLifeContingenciesXyz(n=n,lifecontingency = lifecontingency,
tablesList = tablesList,x=x,t=t,m=m,k=k,status=status, 
		parallel=FALSE)
APV
mean(samples)
}
