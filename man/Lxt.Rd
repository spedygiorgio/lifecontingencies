\name{Lxt}
\alias{Lxt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Number of person - years lived.
}
\description{
	This function calculates the Lxt demographic function.
}
\usage{
Lxt(object, x, t = 1, fxt = 0.5)
}

\arguments{
  \item{object}{
A lifetable object.
}
  \item{x}{
	Age.
}
  \item{t}{
	Length of the period.
}
  \item{fxt}{separation factor, default value is 0.5 (half year).
}
}
\details{
The separation factor is the average number of years not 
lived between exact ages x and x+t for those who die between exact ages x and +t 
}
\value{
An integer value
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, 
	N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}
\note{
This function is used in life tables analysis
}
\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}
\seealso{
\code{\link{Tx}}, \code{\linkS4class{lifetable}}
}
\examples{
		#assumes SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		Lxt(soa08Act, 67,10)
}