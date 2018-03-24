\name{axyn}
\alias{axyn}
\alias{Axyn}
\title{
Functions to evaluate life insurance and annuities on two heads.
}
\description{
These functions evaluates life insurances and annuities on two heads.
}
\usage{
axyn(tablex, tabley, x, y, n, i, m, k = 1, status = "joint", type = "EV", 
payment="advance")
Axyn(tablex, x, tabley, y, n, i, m, k = 1, status = "joint", type = "EV")
}

\arguments{
  \item{tablex}{Life X \code{lifetable} object.}
  \item{tabley}{Life Y \code{lifetable} object.}
  \item{x}{Age of life X.}
  \item{y}{Age of life Y.}
  \item{n}{Insured duration. Infinity if missing.}
  \item{i}{Interest rate. Default value is those implied in \code{actuarialtable}.}
  \item{m}{Deferring period. Default value is zero.}
  \item{k}{Fractional payments or periods where insurance is payable.}
  \item{status}{Either "joint" or "last" survival status.}
    \item{type}{
	A string, either \code{"EV"} for expected value of the actuarial present value (default) or 
	\code{"ST"} for one stochastic realization of the underlying present value of benefits.
	Alternatively, one can use \code{"expected"} or \code{"stochastic"}
	respectively (can be abbreviated).
}
  \item{payment}{
The Payment type, either \code{"advance"} for the annuity due (default)
or \code{"arrears"} for the annuity immediate.
Alternatively, one can use \code{"due"} or \code{"immediate"}
	respectively (can be abbreviated).
}
}

\details{
Actuarial mathematics book formulas has been implemented.
}
\value{
A numeric value returning APV of chosen insurance form.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}
\note{
Deprecated functions. Use \code{Axyzn} and \code{axyzn} instead.
}

\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy 
	of calculations. The author disclaims any liability for eventual losses 
	arising from direct or indirect use of this software.
}

\seealso{
\code{\link{pxyt}}
}
\examples{
\dontrun{
	data(soa08Act)
	#last survival status annuity
	axyn(tablex=soa08Act, tabley=soa08Act, x=65, y=70, 
		n=5,  status = "last",type = "EV")
    #first survival status annuity
	Axyn(tablex=soa08Act, tabley=soa08Act, x=65, y=70,
	status = "last",type = "EV")
	}
}
