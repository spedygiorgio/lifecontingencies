\name{multiple life probabilities}
\alias{exyzt}
\alias{pxyzt}
\alias{qxyzt}
\title{
Functions to deals with multiple life models
}
\description{
These functions evaluate multiple life survival probabilities, either for joint or last life status. Arbitrary life probabilities can 
be generated as well as random samples of lifes.
}
\usage{
exyzt(tablesList, x, t = Inf, status = "joint",  type = "Kx", \dots)

pxyzt(tablesList, x, t, status = "joint", 
fractional=rep("linear", length(tablesList)), \dots)

qxyzt(tablesList, x, t, status = "joint",  
fractional=rep("linear",length(tablesList)), \dots)

}

\arguments{
  \item{tablesList}{A list whose elements are either \code{lifetable} or \code{actuarialtable} class objects.}
  \item{x}{A vector of the same size of tableList that contains the initial ages.}
  \item{t}{The duration.}
  \item{status}{Either \code{"joint"} for the joint-life status model or \code{"last"}
for the last-survivor status model (can be abbreviated).}
  \item{type}{Either \code{"Tx"} for continuous future lifetime, \code{"Kx"} for 
curtate furture lifetime (can be abbreviated).}
\item{fractional}{Assumptions for fractional age. One of \code{"linear"}, 
  \code{"hyperbolic"}, \code{"constant force"} (can be abbreviated).}
  
\item{\dots}{Options to be passed to \code{\link{pxt}}.}
\item{k}{Fractional frequency option.}
}
\details{
These functions extends \code{\link{pxyt}} family to an arbitrary number of life contingencies.
}
\value{
An estimate of survival / death probability or expected lifetime, or a matrix of ages.
}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
Giorgio Alfredo, Spedicato
}
\note{
The procedure is experimental.
}
\seealso{
\code{\link{pxt}},\code{\link{exn}}
}
\examples{
#assessment of curtate expectation of future lifetime of the joint-life status
#generate a sample of lifes
data(soaLt)
soa08Act=with(soaLt, new("actuarialtable",interest=0.06,x=x,lx=Ix,name="SOA2008"))
tables=list(males=soa08Act, females=soa08Act)
xVec=c(60,65)
test=rLifexyz(n=50000, tablesList = tables,x=xVec,type="Kx")
#check first survival status
t.test(x=apply(test,1,"min"),mu=exyzt(tablesList=tables, x=xVec,status="joint"))
#check last survival status
t.test(x=apply(test,1,"max"),mu=exyzt(tablesList=tables, x=xVec,status="last"))
}

