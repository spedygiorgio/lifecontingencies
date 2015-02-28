\name{demoUsa}
\alias{demoUsa}
\docType{data}
\title{United States Social Security life tables
}
\description{
This data set contains period life tables for years 1990, 2000 and 2007. Both males and females 
life tables are reported.
}
\usage{data(demoUsa)}
\format{
  A data frame with 114 observations on the following 7 variables.
  \describe{
    \item{\code{age}}{age vector}
    \item{\code{USSS2007M}}{2007 male life table}
    \item{\code{USSS2007F}}{2007 female life table}
    \item{\code{USSS2000M}}{2000 male life table}
    \item{\code{USSS2000F}}{2000 female life table}
    \item{\code{USSS1990M}}{1990 male life table}
    \item{\code{USSS1990F}}{1990 female life table}
  }
}
\details{
	Reported age is truncated at the last age with \eqn{lx>0}.
}
\source{
	See \url{http://www.ssa.gov/oact/NOTES/as120/LifeTables_Body.html}
}
\references{
	Social Security Agency.
}


\examples{
data(demoUsa)
head(demoUsa)
}
\keyword{datasets}
