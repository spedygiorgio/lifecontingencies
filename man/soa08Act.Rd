\name{soa08Act}
\alias{soa08Act}
\docType{data}
\title{
Society of Actuaries tables to perform life contingencies calculations.
}
\description{
An object of actuarialtable built from SOA life table. Interest rate is 6%.
}
\usage{data(soa08Act)}
\format{
  The format is:
Formal class 'actuarialtable' [package ".GlobalEnv"] with 4 slots
  ..@ interest: num 0.06
  ..@ x       : int [1:95] 0 5 10 15 20 21 22 23 24 25 ...
  ..@ lx      : int [1:95] 10000000 9749503 9705588 9663731 9617802 9607896 9597695 9587169 9576288 9565017 ...
  ..@ name    : chr "SOA2008"
}
\details{
Early ages are miss.
}
\source{
\url{http://www.soa.org/files/pdf/edu-2008-spring-mlc-tables.pdf}
}
\references{
Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
}
\examples{
\dontrun{
	data(soa08Act)
}
}
\keyword{datasets}
