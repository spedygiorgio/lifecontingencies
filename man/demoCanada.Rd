\name{demoCanada}
\alias{demoCanada}
\docType{data}
\title{Canada Mortality Rates for UP94 Series}
\description{UP94 life tables underlying mortality rates}
\usage{data(demoCanada)}
\format{
  A data frame with 120 observations on the following 7 variables.
  \describe{
    \item{\code{x}}{age}
    \item{\code{up94M}}{UP 94, males}
    \item{\code{up94F}}{UP 94, females}
    \item{\code{up942015M}}{UP 94 projected to 2015, males}
    \item{\code{up942015f}}{UP 94 projected to 2015, females}
    \item{\code{up942020M}}{UP 94 projected to 2020, males}
    \item{\code{up942020F}}{UP 94 projected to 2020, females}
  }
}
\details{Mortality rates are provided.}
\source{Courtesy of Andrew Botros}
\references{Courtesy of Andrew Botros}
\examples{
data(demoCanada)
head(demoCanada)
#create the up94M life table
up94MLt<-probs2lifetable(probs=demoCanada$up94M,radix=100000,"qx",name="UP94")
#create the up94M actuarial table table
up94MAct<-new("actuarialtable", lx=up94MLt@lx, x=up94MLt@x,interest=0.02)
}
\keyword{datasets}