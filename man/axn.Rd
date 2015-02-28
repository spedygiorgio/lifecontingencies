\name{axn}
\alias{axn}

\title{
Annuity immediate and due function.
}
\description{
	This function calculates actuarial value of annuities, given an actuarial table. 
	Fractional and deferred annuities can be evaluated.	Moreover it can be used to 
	simulate the stochastic distribution of the annuity value.
}
\usage{
	axn(actuarialtable, x, n, i = actuarialtable@interest, m,  k = 1, type = "EV",
	power=1,payment = "advance")
}
	
\arguments{
  \item{actuarialtable}{
	An actuarial table object.
}
  \item{x}{
	Age of the annuitant.
}
  \item{n}{
	Number of terms of the annuity, if missing annuity is intended to be paid until death.
}
  \item{i}{
	Interest rate (default value the interest of the life table).
}
  \item{m}{
	Deferring period. Assumed to be 1 whether missing.
}
  \item{k}{
	Number of fractional payments per period. Assumed to be 1 whether missing.
}
  \item{type}{
	A string, eithed "EV" (default value) or "ST" (stochastic realization).
}
  \item{power}{
The power of the APV. Default is 1 (mean)
}
  \item{payment}{
Payment type: "advance" default is the annuity due, otherwise annuity immediate.
}
}
\details{
	When "ST" has been selected a stochastic value representing a number drawn from the domain of \deqn{
	a_{x}^{n}} is drawn. "EV" calculates the classical APV.
}
\value{
	A numeric value.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
	Giorgio A. Spedicato
}
\note{
	When either \eqn{x=\omega} or \eqn{n=0} zero is returned.
}

\section{Warning}{
	The function is provided as is, without any warranty regarding the accuracy of calculations. 
	The author disclaims any liability for eventual losses arising from direct or indirect 
	use of this software.
}
\seealso{
	\code{\link{annuity}}, \code{\link{Exn}}
}
\examples{
		#assume SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate and life-long annuity for an aged 65
		axn(soa08Act, x=65) 
}

