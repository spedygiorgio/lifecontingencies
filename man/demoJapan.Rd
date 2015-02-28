\name{demoJapan}
\alias{demoJapan}
\docType{data}
\title{
Japan Mortality Rates for life table construction
}
\description{
Two yearly mortality rates for each age
}
\usage{data(demoJapan)}
\format{
  A data frame with 110 observations on the following 3 variables.
  \describe{
    \item{\code{age}}{Attained age}
    \item{\code{JP8587M}}{Male life table}
    \item{\code{JP8587F}}{Female life table}
  }
}
\details{
See the references link for details.
}
\source{
Society of Actuaries mortality web site
}
\references{
\url{www.mort.soa.org}
}
\examples{
\dontrun{data(demoJapan)
head(demoJapan)}
}
\keyword{datasets}
