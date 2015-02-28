\name{Iaxn}
\alias{Iaxn}
\title{
Increasing annuity life contingencies
}
\description{
This function evaluates increasing annuities
}
\usage{
Iaxn(actuarialtable, x, n, i, m = 0, type = "EV", power=1)
}
\arguments{
  \item{actuarialtable}{
An \code{actuarialtable} object.
}
  \item{x}{
The age of the insured head.
}
  \item{n}{
The duration of the insurance
}
  \item{i}{
The interest rate that overrides the one in the \code{actuarialtable} object.
}
  \item{m}{
The deferring period.
}
  \item{type}{
Yet only "EV" is implemented.
}
  \item{power}{
The power of the APV. Default is 1 (mean)
}
}
\details{
This actuarial mathematics is generally exoteric. I have seen no valid example of it.
}
\value{
The APV of the insurance}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}
\note{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.
}
\section{Warning }{The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual losses arising from direct or indirect use of this software.}

\seealso{\code{\link{axn}},\code{\link{IAxn}}}
\examples{
		#using SOA illustrative life tables
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate the value of a lifetime increasing annuity for a subject aged 80
		Iaxn(actuarialtable=soa08Act, x=80, n=10)
}
