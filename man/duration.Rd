\name{duration}
\alias{duration}
\alias{convexity}
\title{
Functions to evaluate duration and convexity}
\description{
These functions evaluate the duration or the convexity of a series of cash flows}
\usage{

duration(cashFlows, timeIds, i, k = 1, macaulay = TRUE)

convexity(cashFlows, timeIds, i, k = 1)
}
\arguments{

  \item{cashFlows}{
A vector representing the cash flows amounts.
}
  \item{timeIds}{Cash flows times
}
  \item{i}{
APR interest, i.e. nominal interest rate compounded m-thly.
}
  \item{k}{
Compounding frequency for the nominal interest rate \eqn{i}.
}
  \item{macaulay}{
Is the macaulay duration (default value) or the effective duration to be evaluated?
}

}
\details{

The Macaulay duration is defined 
as \eqn{\sum\limits_t^{T} \frac{t*CF_{t}\left( 1 + \frac{i}{k} \right)^{ - t*k}}{P}}, while 
\eqn{\sum\limits_{t}^{T} t*\left( t + \frac{1}{k} \right) * CF_t \left(1 + \frac{y}{k} \right)^{ - k*t - 2}}

}
\value{
A numeric value representing either the duration or the convexity of the cash flow series}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
Giorgio A. Spedicato
}
\note{
Vectorial interest rate are not handled yet.
}

\seealso{
\code{\link{annuity}}
}
\examples{
#evaluate the duration of a coupon payment
cf=c(10,10,10,10,10,110)
t=c(1,2,3,4,5,6)
duration(cf, t, i=0.03)
#and the convexity

convexity(cf, t, i=0.03)
}

