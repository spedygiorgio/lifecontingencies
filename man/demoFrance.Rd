\name{demoFrance}
\alias{demoFrance}
\docType{data}
\title{
French population life tables
}
\description{
Illustrative life tables from French population.
}
\usage{data(demoFrance)}
\format{
  A data frame with 113 observations on the following 5 variables.
  \describe{
    \item{\code{age}}{Attained age}
    \item{\code{TH00_02}}{Male 2000 life table}
    \item{\code{TF00_02}}{Female 2000 life table}
    \item{\code{TD88_90}}{1988 1990 life table}
    \item{\code{TV88_90}}{1988 1990 life table}
  }
}
\details{
These tables are real French population life tables. They regard
88 - 90 and 00 - 02 experience.
}
\source{
	\url{http://www.actuaris.com/fr/infotech/actuariat/tables_de_mortalit-/table_tv_88-90.html}
	\url{http://www.winter-associes.fr/ressources/TM-INSEE-TH00-02-TF00-02}
}
\references{
\url{http://www.winter-associes.fr/ressources/TM-INSEE-TH00-02-TF00-02}
}
\examples{
data(demoFrance)
head(demoFrance)
}
\keyword{datasets}
