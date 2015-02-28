\name{Uk life tables}
\alias{AM92Lt}
\alias{AF92Lt}
\docType{data}
\title{Uk AM AF 92 life tables}
\description{
Uk AM AF life tables
}
\usage{data(AF92Lt)}
\format{
  The format is:
Formal class 'lifetable' [package ".GlobalEnv"] with 3 slots
  ..@ x   : int [1:111] 0 1 2 3 4 5 6 7 8 9 ...
  ..@ lx  : num [1:111] 100000 99924 99847 99770 99692 ...
  ..@ name: chr "AF92"
}
\details{
Probabilities for earliest (under 16) and lastest ages (over 92) have 
been derived using a Brass - Logit model fit on Society of Actuaries life table.
}
\source{
See Uk life table.
}
\references{
\url{http://www.actuaries.org.uk/research-and-resources/documents/am92-permanent-assurances-males}
}
\examples{
data(AF92Lt)
exn(AF92Lt)
data(AM92Lt)
exn(AM92Lt)
}         
\keyword{datasets}





