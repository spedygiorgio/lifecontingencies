\name{probs2lifetable}
\alias{probs2lifetable}
\title{
Life table from probabilities
}
\description{
This function returns a newly created lifetable object given either 
survival or death (one year) probabilities) 
}
\usage{
probs2lifetable(probs, radix = 10000, type = "px", name = "ungiven")
}

\arguments{
  \item{probs}{
	A real valued vector representing either one year survival or death probabilities.
	The last value in the vector must be either 1 or 0, depending if it represents 
	death or survival probabilities respectively.
}
  \item{radix}{
The radix of the life table.
}
  \item{type}{
Character value either "px" or "qx" indicating how probabilities must be interpreted.
}
  \item{name}{
The character value to be put in the corresponding slot of returned object.
}
}
\details{
The \eqn{\omega} value is the length of the probs vector. 
}
\value{
A \code{\linkS4class{lifetable}} object.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}
\note{
This function allows to use mortality projection given by other softwares with the lifecontingencies package.
}

\section{Warning }{	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.}

\seealso{
\code{\linkS4class{actuarialtable}}
}
\examples{
fakeSurvivalProbs=seq(0.9,0,by=-0.1)
newTable=probs2lifetable(fakeSurvivalProbs,type="px",name="fake")
head(newTable)
tail(newTable)
}
