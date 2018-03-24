\name{exn}
\alias{exn}

\title{
Function/method to calculate the expected life.
}
\description{
This method calculates the expected life span between ages x and x+n.
}
\usage{
exn(object, x, n,type="curtate")
}

\arguments{
  \item{object}{
A lifetable object.
}
  \item{x}{
Age from which the life span should be calculated.
}
  \item{n}{
Time until which the expected life should be calculated. Assumed omega - x whether missing.
}
\item{type}{Either \code{"Tx"}, \code{"complete"} or \code{"continuous"} for continuous future lifetime, 
\code{"Kx"} or \code{"curtate"} for curtate furture lifetime (can be abbreviated).}
}
\value{
A numeric value representing the expected life span.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato.
}

\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\linkS4class{lifetable}}
}

\examples{


	#loads and show
	data(soa08Act)
	exn(object=soa08Act, x=0)
	exn(object=soa08Act, x=0,type="complete")
	

}
