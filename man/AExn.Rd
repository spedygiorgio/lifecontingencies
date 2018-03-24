\name{AExn}
\alias{AExn}

\title{
Function to evaluate the n-year endowment insurance
}
\description{
This function evaluates the n-year endowment insurance.
}
\usage{
AExn(actuarialtable, x, n, i=actuarialtable@interest,  k = 1, type = "EV", power=1)
}
\arguments{
  \item{actuarialtable}{
	An actuarial table object.
}
  \item{x}{
Insured age.
}
  \item{n}{
Length of the insurance.
}
  \item{i}{
Rate of interest. When missing the one included in the actuarialtable object is used.
}
  \item{k}{
Frequency of benefit payment.
}
  \item{type}{
	A string, either \code{"EV"} for expected value of the actuarial present value (default) or 
	\code{"ST"} for one stochastic realization of the underlying present value of benefits.
	Alternatively, one can use \code{"expected"} or \code{"stochastic"}
	respectively (can be abbreviated).
}
  \item{power}{
The power of the APV. Default is 1 (mean)
}
}
\details{
The n-year endowment insurance provides a payment either in the year of death or at the end of 
the insured period.
}
\value{
A numeric value.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}
\note{
When type="EV" the function calls both \code{Axn} and \code{Exn}. 
}

\seealso{
\code{\link{Axn}},\code{\link{Exn}}
}
\examples{
#Actuarial Mathematics book example
#check the actuarial equality on the expected values Exn+Axn=AExn
data(soa08Act)
AExn(soa08Act, x=35,n=30,i=0.06)
Exn(soa08Act, x=35,n=30,i=0.06)+Axn(soa08Act, x=35,n=30,i=0.06)
}
