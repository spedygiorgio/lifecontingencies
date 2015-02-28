\name{mxt}
\alias{mxt}

\title{
Central mortality rate.
}
\description{
This function returns the central mortality rate demographic function.
}
\usage{
mxt(object, x, t)
}

\arguments{
  \item{object}{
A lifetable object
}
  \item{x}{
Age when the calculation starts.
}
  \item{t}{
Age when the calculation ends, default=1.
}
}

\details{
	The central mortality rate is defined as \eqn{m_{x,t}=\frac{d_{x,t}}{{l_{x,t}}}}
}
\value{
A numeric value representing the central mortality rate between age x and x+t.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, 
	N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}
\note{
This function is used in demographic analysis.
}
\seealso{
\code{\linkS4class{lifetable}}, \code{\link{Lxt}}
}
\examples{
		#assumes SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#compare mx and qx 
		mxt(soa08Act, 60,10)
		qxt(soa08Act, 60,10)
}