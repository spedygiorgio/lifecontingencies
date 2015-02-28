\name{DAxn}
\alias{DAxn}
\title{Decreasing life insurance}
\description{
	This function evaluates the n-year term decreasing life insurance. 
	Both actuarial value and stochastic random sample can be returned.
}
\usage{
	DAxn(actuarialtable, x, n, 
	i=actuarialtable@interest,m = 0,k=1, 
	type = "EV", power=1)
}
\arguments{
  \item{actuarialtable}{
	An actuarial table object.
}
  \item{x}{
	Age of the insured.
}
  \item{n}{
	Length of the insurance period.
}
  \item{i}{
	Interest rate, when present it overrides the interest rate of the actuarial table object.
}
  \item{m}{
	Deferring period, even fractional, assumed 1 whether missing.
}
  \item{k}{
  Number of fractional payments per period. Assumed to be 1 whether missing.
}
  \item{type}{
Default value is "EV", where APV is returned. "ST" returns a sample from the underlying 
present value of benefits distribution.
}
  \item{power}{
The power of the APV. Default is 1 (mean)
}
}
\details{
	Formulas of Bowes book have been implemented.
}
\value{
	A numeric value representing the expected value or the simulated value.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
	Giorgio A. Spedicato
}
\note{Neither fractional payments nor stochastic calculations have been implemented yet.}
\section{Warning}{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.}
\seealso{\code{\link{Axn}},\code{\link{IAxn}}}
\examples{
		#using SOA illustrative life tables
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate the value of a 10 years decreasing term life insurance for an aged 25
		DAxn(actuarialtable=soa08Act, x=25, n=10) 
}

