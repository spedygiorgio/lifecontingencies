\name{Axyzn}
\alias{Axyzn}
\alias{axyzn}
\title{
Multiple lifes insurances and annuities
}
\description{
Function to evalate the multiple lives insurances and annuities
}
\usage{
Axyzn(tablesList, x, n, i, m, k = 1, status = "joint", type = "EV", 
power=1)
axyzn(tablesList, x, n, i, m, k = 1, status = "joint", type = "EV", 
power=1, payment="advance")
}

\arguments{
  \item{tablesList}{
A list whose elements are either lifetable or actuarialtable class objects.
}
  \item{x}{
A vector of the same size of tableList that contains the initial ages.
}
  \item{n}{
Lenght of the insurance.
}
  \item{i}{
Interest rate
}
  \item{m}{
Deferring period.
}
  \item{k}{
Fractional payment frequency.
}
  \item{status}{Either \code{"joint"} for the joint-life status model or \code{"last"}
for the last-survivor status model (can be abbreviated).}

  \item{type}{
	A string, either \code{"EV"} for expected value of the actuarial present value (default) or 
	\code{"ST"} for one stochastic realization of the underlying present value of benefits.
	Alternatively, one can use \code{"expected"} or \code{"stochastic"}
	respectively (can be abbreviated).
}
  \item{power}{
The power of the APV. Default is 1 (mean).
}
  \item{payment}{
The Payment type, either \code{"advance"} for the annuity due (default)
or \code{"arrears"} for the annuity immediate.
Alternatively, one can use \code{"due"} or \code{"immediate"}
	respectively (can be abbreviated).
}
}
\details{
In theory, these functions apply the same concept of life insurances 
on one head on multiple heads.
}
\value{
The insurance value is returned.
}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
Giorgio Alfredo Spedicato, Kevin J. Owens.
}
\note{
These functions are the more general version of \code{\link{axyn}} and 
\code{\link{Axyn}}.
}

\seealso{
\code{\link{axyn}},\code{\link{Axyn}}.
}
\examples{
	data(soaLt)
	soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
	x=x,lx=Ix,name="SOA2008"))
	#evaluate and life-long annuity for an aged 65
	listOfTables=list(soa08Act, soa08Act) 
	#Check actuarial equality
	axyzn(listOfTables,x=c(60,70),status="last")
	axn(listOfTables[[1]],60)+axn(listOfTables[[2]],70)-
	axyzn(listOfTables,x=c(60,70),status="joint")	
}

