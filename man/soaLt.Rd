\name{soaLt}
\alias{soaLt}
\docType{data}
\title{
Society of Actuaries life table.
}
\description{
This table has been used by the classical book Actuarial Mathematics and by the Society of Actuaries for US professional examinations.
}
\usage{data(soaLt)}
\format{
  A data frame with 95 observations on the following 2 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{Ix}}{a numeric vector}
  }
}
\details{
Early ages are miss.
}
\references{
Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
}

\note{
SOA has not provided population at risk data for certain spans of age (e.g. 1-5, 6-9, 11-14 and 16-19).
Linear interpolation has been used to fill them.
}

\examples{
data(soaLt)
head(soaLt)
}
\keyword{datasets}
