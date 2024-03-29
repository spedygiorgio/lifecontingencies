% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{demoUk}
\alias{demoUk}
\title{UK life tables}
\format{
A data frame with 74 observations on the following 3 variables:
\describe{ \item{\code{Age}}{Annuitant age} \item{\code{AM92}}{One year mortality rate (males)} \item{\code{AF92}}{One year mortality rate (males)} }
}
\source{
Institute of Actuaries
}
\usage{
data(demoUk)
}
\description{
AM and AF one year mortality rate. Series of 1992
}
\details{
This data set shows the one year survival rates for males and females 
of the 1992 series. It has been taken from the Institute of Actuaries. 
The series cannot be directly used to create a life table since neither 
rates are not provided for ages below 16 nor for ages over 90. Various 
approach can be used to complete the series.
}
\examples{
data(demoUk)
head(demoUk)
}
\references{
\url{https://www.actuaries.org.uk/learn-and-develop/continuous-mortality-investigation/cmi-mortality-and-morbidity-tables/92-series-tables}
}
\keyword{datasets}
