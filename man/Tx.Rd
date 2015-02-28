\name{Tx}
\alias{Tx}

\title{
Number of person-years lived after exact age x
}
\description{
This function returns the number of years lived after exact age x.
}
\usage{
Tx(object, x)
}

\arguments{
  \item{object}{
A lifetable object
}
  \item{x}{
	An integer representing the age for which the \eqn{T_x} value shall be returned.
}
}
\details{
	x shall be an integer value.
}
\value{
	A numeric value.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, 
	by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}
\note{
The calculation is performed on all x where \eqn{l_x>0}. 
}
\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\link{Lxt}}
}
\examples{

		#assumes SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		Tx(soa08Act, 67)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
