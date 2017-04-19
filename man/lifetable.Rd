\name{lifetable}
\alias{lifetable}

\title{
Constructor for lifetable-class objects
}
\description{
Constructs an object of class lifetable
}
\usage{
lifetable(x = 0:3, lx = c(100, 90, 50, 10), name = "Generic life table")
}

\arguments{
  \item{x}{
    A vector of type \code{"numeric"} representing the ages of the population
}
  \item{lx}{
  A vector of type \code{"numeric"} representing the number of lives at 
  the beginning of age \eqn{x}.  It is a non-increasing sequence with its
  last entry greater than zero.
}
  \item{name}{
  A \code{"character"} vector reporting the name of the table
}
}
\details{
  Vector \code{x} and \code{lx} must be of equal length.  
  
  Zero and NA entries in vector \code{lx} will be removed
  (along with corresponding entries in \code{x}).
  
  Entries in \code{x} will be sorted in increasing order (same
  ordering will be applied to \code{lx}).
}
\value{
  An object of class \code{"lifetable"}
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Ernesto Schirmacher
}

\section{Warning }{
	The function is provided as is, without any warranty regarding 
	the accuracy of calculations. The author disclaims any liability 
	for eventual losses arising from direct or indirect use of
	this software.
}
\seealso{
  \code{\linkS4class{actuarialtable}}
}
\examples{
ages <- 0:9
pop  <- seq(100, 10, by = - 10)
tbl <- lifetable(x = ages, lx = pop, name = "Uniformly decreasing population at risk")
summary(tbl)
head(tbl)
}
