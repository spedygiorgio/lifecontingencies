\name{soa08}
\alias{soa08}
\docType{data}
\title{
Society of Actuaries Illustrative Life Table object.
}
\description{
This is the table that appears in the classical book
Actuarial Mathematics in Appendix 2A and used throughout the
book to illustrate life contingent calculations.
The Society of Actuaries has been using this table when administering
US actuarial professional MLC preliminary examinations.
}
\usage{data(soa08)}
\format{
  Formal class 'lifetable' [package "lifecontingencies"] with 3 slots
  ..@ x   : int [1:141] 0 1 2 3 4 5 6 7 8 9 ...
  ..@ lx  : num [1:141] 100000 97958 97826 97707 97597 ...
  ..@ name: chr "SOA Illustrative Life Table"
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
\note{
This table is based on US 1990 general population mortality.
}
\references{
Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
}
\examples{
data(soa08)
## maybe str(soa08) ; plot(soa08) ...
}
\keyword{datasets}
