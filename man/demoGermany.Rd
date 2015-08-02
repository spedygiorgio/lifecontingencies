\name{demoGermany}
\alias{demoGermany}
\docType{data}
\title{
German population life tables
}
\description{
Dataset containing mortality rates for German population, male and females.
}
\usage{data(demoGermany)}
\format{
  A data frame with 113 observations on the following 5 variables.
  \describe{
    \item{\code{x}}{Attained age}
    \item{\code{qxMale}}{Male mortality rate}
    \item{\code{qxFemale}}{Female mortality rate}
  }
}
\details{
Sterbetafel DAV 1994
}
\source{
Private communicatiom
}
\examples{
data(demoGermany)
head(demoGermany)
}
\keyword{datasets}
