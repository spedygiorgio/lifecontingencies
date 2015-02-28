\name{SoAISTdata}
\alias{SoAISTdata}
\docType{data}
\title{SoA illustrative service table}
\description{Bowers' book Illustrative Service Table}
\usage{data(SoAISTdata)}
\format{
  A data frame with 41 observations on the following 6 variables.
  \describe{
    \item{\code{x}}{Attained age}
    \item{\code{lx}}{Surviving subjects ate the beginning of each age}
    \item{\code{death}}{Drop outs for death cause}
    \item{\code{withdrawal}}{Drop outs for withdrawal cause}
    \item{\code{inability}}{Drop outs for inability cause}
    \item{\code{retirement}}{Drop outs for retirement cause}
  }
}
\details{
It is a data frame that can be used to create a multiple decrement table
}
\source{
Optical recognized characters from below source with some few adjustments
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\examples{
data(SoAISTdata)
head(SoAISTdata)
}
\keyword{datasets}
