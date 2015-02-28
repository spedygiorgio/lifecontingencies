\name{demoChina}
\alias{demoChina}
\docType{data}
\title{
China Mortality Rates for life table construction
}
\description{
Seven yearly mortality rates for each age
}
\usage{data(demoChina)}
\format{
  A data frame with 106 observations on the following 8 variables.
  \describe{
    \item{\code{age}}{Attained age}
    \item{\code{CL1}}{CL1 rates}
    \item{\code{CL2}}{CL2 rates}
    \item{\code{CL3}}{CL3 rates}
    \item{\code{CL4}}{CL4 rates}
    \item{\code{CL5}}{CL5 rates}
    \item{\code{CL6}}{CL6 rates}
    \item{\code{CL90-93}}{CL 90-93 rates}
  }
}
\details{
See the source link for details.
}
\source{
Society of Actuaries
}
\references{
\url{www.mort.soa.org}
}
\examples{
data(demoChina)
tableChinaCL1<-probs2lifetable(probs=demoChina$CL1,radix=1000,type="qx",name="CHINA CL1")
}
\keyword{datasets}
