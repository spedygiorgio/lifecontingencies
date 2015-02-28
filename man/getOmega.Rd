\name{getOmega}
\alias{getOmega}

\title{
Function to return the terminal age of a life table.
}
\description{
This function returns the \eqn{\omega} value of a life table object, that is,
the last attainable age within a life table.
}
\usage{
getOmega(object)
}
\arguments{
  \item{object}{A life table object.}
}
\value{
A numeric value representing the \eqn{\omega} value of a life table object}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}
\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\linkS4class{actuarialtable}}
}
\examples{
		#assumes SOA example life table to be load
		data(soaLt)
		soa08=with(soaLt, new("lifetable",
		x=x,lx=Ix,name="SOA2008"))
		#the last attainable age under SOA life table is
		getOmega(soa08) 

}

