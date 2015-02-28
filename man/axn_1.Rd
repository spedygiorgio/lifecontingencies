\name{Axn}
\alias{Axn}

\title{
	Function to evaluate life insurance.
}
\description{
	This function evaluates n - years term and whole life insurance.
}
\usage{
	Axn(actuarialtable, x, n, i=actuarialtable@interest,m, k=1, type = "EV",power=1)
}

\arguments{
  \item{actuarialtable}{
	An actuarial table object.
}
  \item{x}{
	Age of the insured.
}
  \item{n}{
	Coverage period, if missing the insurance is considered whole life \eqn{n=\omega-x-m}
}
  \item{i}{
	Interest rate (overrides the interest rate slot in \code{actuarialtable}).
}
  \item{m}{
	Deferring period, even fractional, if missing assumed to be 0.
}
  \item{k}{
	Number of periods per year at the end of which the capital is payable in case of 
	insured event, default=1 (capital payable at the end of death year).
}
  \item{type}{
	A character value, either "EV" (default value) or "ST".
}
  \item{power}{
The power of the APV. Default is 1 (mean)
}
}
\details{
	The variance calculation has not been implemented yet.
}
\value{
	A numeric value representing either the actuarial value of the coverage (when type="EV") or 
	a number drawn from the underlying distribution of Axn.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
	Giorgio A. Spedicato
}
\note{
	It is possible that value returned by stochastic simulation are biased. Successive releases of this software will analyze 
	the issue with detail.
}
\section{Warning}{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
	\code{\link{axn}}, \code{\link{Exn}}
}
\examples{
	
		#assume SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate the value of a 40 years term life insurance for an aged 25
		Axn(actuarialtable=soa08Act, x=25, n=40) 
		#check an relevant life contingencies relationship
		k=12
		i=0.06
		j=real2Nominal(i,k)
		Axn(soa08Act, 30,k=12)
		i/j*Axn(soa08Act, 30,k=1)
	}

