\name{soa08Act}
\alias{soa08Act}
\docType{data}
\title{
Society of Actuaries Illustrative Life Table with interest rate at 6%.
}
\description{
An object of class actuarialtable built from the SOA illustrative life table. Interest rate is 6%.
}
\usage{data(soa08Act)}
\format{
Formal class 'actuarialtable' [package "lifecontingencies"] with 4 slots
  ..@ interest: num 0.06
  ..@ x       : int [1:141] 0 1 2 3 4 5 6 7 8 9 ...
  ..@ lx      : num [1:141] 100000 97958 97826 97707 97597 ...
  ..@ name    : chr "SOA Illustrative Life Table"
}
\details{
This table is a blend of Makeham's mortality law for ages 13 and above and
some ad hoc values for ages 0 to 12.

The parameters for Makeham's mortality law are

1000 * mu(x) = 0.7 + 0.05 * 10^(0.04 * x)

where mu(x) is the force of mortality.

The published Illustrative Life Table just shows ages 0 to 110 but in the
computing exercises of chapter 3 the authors explain that the table's age
range is from 0 to 140.
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
