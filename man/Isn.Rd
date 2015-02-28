\name{Isn}
\alias{Isn}
\title{
Function to calculated accumulated increasing annuity future value.
}
\description{
	This function evaluates non - stochastic increasing annuities future values.
}
\usage{
Isn(i, n, type = "immediate")
}

\arguments{
  \item{i}{
Interest rate.
}
  \item{n}{
Terms.
}
  \item{type}{
Either "due" for annuity due or "immediate" for annuity immediate.
}
}
\details{
It calls \code{\link{increasingAnnuity}} after having capitalized by \eqn{\left( 1 + i \right)^n}
}
\value{
A numeric value
}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
Giorgio A. Spedicato
}
\note{
	This function calls internally \code{increasingAnnuity} function.
}

\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\link{accumulatedValue}}
}
\examples{
Isn(n=10,i=0.03)
}

