\name{effective2Convertible}
\alias{effective2Convertible}
\alias{convertible2Effective}
\alias{nominal2Real}
\alias{real2Nominal}

\title{
Function to switch from nominal / effective / convertible rates
}
\description{
This function provides convenience functions to switch from effective to convertible rate.
}
\usage{

effective2Convertible(i, k = 1, type = "interest")

convertible2Effective(i, k = 1, type = "interest")

nominal2Real(i, k = 1, type = "interest")

real2Nominal(i, k = 1, type = "interest")
}

\arguments{

  \item{i}{
The rate to be converted.
}
  \item{k}{
The original / target compounting frequency.
}
  \item{type}{
Either "interest" (default) or "nominal".
}
}
\details{
\code{effective2Convertible} and \code{convertible2Effective} wrap the other two functions.
}
\value{
A numeric value.
}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
}
\author{
Giorgio A. Spedicato
}
\note{
Convertible rates are synonims of nominal rates
}
\section{Warning }{The function is provided at is without any guarantee of results.}

\seealso{
\code{\link{real2Nominal}}
}
\examples{
#a nominal rate of 0.12 equates an APR of
nominal2Real(i=0.12, k = 12, "interest")
}

