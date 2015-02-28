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
  \item{status}{
Either "joint" (for the joint-life status model) or "last".
}
  \item{type}{
"EV", for expected value. "ST" for stochastic value.
}
  \item{power}{
The power of the APV. Default is 1 (mean).
}
  \item{payment}{
Payment type: "advance" default is the annuity due, otherwise annuity due.
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

