\name{rLife}
\alias{rLife}
\title{
Function to generate random future lifetimes
}
\description{
This function returns random samples from the time until death (future lifetime) of a subject aged x. Either 
the continuous or the curtate time until death can be returned.
}
\usage{
rLife(n, object, x = 0, k=1, type = "Tx")
}

\arguments{
  \item{n}{
Number of variates to generate.
}
  \item{object}{
An object of class lifetable.
}
  \item{x}{
The attained age of subject x. Default value is 0.
}
  \item{k}{
Number of periods within the year when it is possible death to happen. Default value is 1.
}
  \item{type}{Either \code{"Tx"} for continuous future lifetime, \code{"Kx"} for 
curtate furture lifetime (can be abbreviated).}

  
}
\details{
Following relation holds for the future life time: \eqn{T_x=K_x+0.5}.
}
\value{
A numeric vector of n elements.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{Giorgio A. Spedicato}
\note{
This function will probably will improved in the future.
}


\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}


\seealso{
\code{\linkS4class{lifetable}}, \code{\link{exn}}
}
\examples{
##get 20000 random future lifetimes for the Soa life table at birth
data(soa08Act)
lifes=rLife(n=20000,object=soa08Act, x=0, type="Tx")
##check if the expected life at birth derived from the life table
##is statistically equal to the expected value of the sample
#
t.test(x=lifes, mu=exn(soa08Act, x=0, type="continuous"))

}
