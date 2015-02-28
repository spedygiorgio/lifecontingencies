\name{pxyt}
\alias{exyt}
\alias{pxyt}
\alias{qxyt}

\title{
Functions to evaluate joint survival probabilities.
}
\description{
These functions evaluate survival and death probabilities for two heads.
}
\usage{

exyt(objectx, objecty, x, y, t, status = "joint")

pxyt(objectx, objecty, x, y, t, status = "joint")

qxyt(objectx, objecty, x, y, t,  status = "joint")
}
\arguments{
  \item{objectx}{
\code{lifetable} for life X.
}
  \item{objecty}{
\code{lifetable} for life Y.
}
  \item{x}{
Age of life X.
}
  \item{y}{
Age of life Y.
}
  \item{t}{
Time until survival has to be evaluated.
}

  \item{status}{
Either "joint" or "last".
}
}

\value{
A numeric value representing joint survival probability.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato, Kevin J. Owens.
}
\note{
These functions are used to evaluate two or more life contingencies.
}

\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\link{exyt}}
}
\examples{
data(soa08Act)
pxyt(soa08Act, soa08Act, 65, 70,10)
pxyt(soa08Act, soa08Act, 65, 70,10, "last")

}

