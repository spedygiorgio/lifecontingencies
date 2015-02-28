\name{Exn}
\alias{Exn}

\title{
	Function to evaluate the pure endowment.
}
\description{
	Given an actuarial table, this function evaluate the pure endowment. 
}
\usage{Exn(actuarialtable, x, n, i=actuarialtable@interest, type = "EV", power=1)}

\arguments{
 \item{actuarialtable}{
	An actuarial table object.
}
  \item{x}{
	Age of the insured.
}
  \item{n}{
	Length of the pure endowment.
}
  \item{i}{
	Interest rate (overrides the interest rate of the actuarial table object)
}
  \item{type}{
	A string, eithed "EV" (default value),  "ST" (stocastic realization) or "VR" if the value of the variance is needed.
}
  \item{power}{
The power of the APV. Default is 1 (mean)
}
}
\details{
As done in all package, interest rate is assumed fixed.
}
\value{
The value of the pure endowmnet.
}

\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}

\seealso{
\code{\link{axn}}
}
\examples{

		#assumes SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate the pure endowment for a man aged 30 for a time span of 35
		Exn(soa08Act, x=30, n=35) 


}

