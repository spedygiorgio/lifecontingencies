\name{rLifeContingencies}
\alias{rLifeContingencies}
\title{Function to generate samples from the life contingencies stochastic variables.}
\description{This function returns a n-size sample from the underlying present value of benefits 
stochastic variable defined by a specific life contingencies insurance form.}
\usage{
rLifeContingencies(n, lifecontingency, object, x, t, 
i = object@interest, m=0, k = 1 , parallel=FALSE, payment="advance")
}

\arguments{
\item{n}{Size of sample}
\item{lifecontingency}{A character string, either \code{"Exn"}, \code{"Axn"}, 
\code{"axn"}, \code{"IAxn"} or \code{"DAxn"}.}
\item{object}{An \code{actuarialtable} object.}
\item{x}{Policyholder's age at issue time.}
\item{t}{The lenght of the insurance. Must be specified according to the present 
value of benefits definition.}
\item{i}{The interest rate, whose default value is the \code{actuarialtable} interest rate slot value.}
\item{m}{Deferring period, default value is zero.}
\item{k}{Fractional payment, default value is 1.}
\item{parallel}{Uses the parallel computation facility.}
\item{payment}{
The Payment type, either \code{"advance"} for the annuity due (default)
or \code{"arrears"} for the annuity immediate.
Alternatively, one can use \code{"due"} or \code{"immediate"}
	respectively (can be abbreviated).
}
}
\details{
This function is a wrapper for internal function that returns the present value of insured 
benefits.
}
\value{A numeric vector.}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{Giorgio Alfredo Spedicato.}
\note{
This function is a wrapper for many internal functions. It is called by all actuarial mathematics functions when value "ST" is provided to 
\code{type} parameter.
}

\section{Warning }{
	Before using this function, the unbiaseness of the sample drawn from the distribution shall be verified. The function is still in testing ad for some classes of life contingencies biased.
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software. Currently k>1 computation are not supported yet.
	}

\seealso{
\code{\link{Exn}}, \code{\link{Axn}}, \code{\link{axn}},\code{\link{IAxn}},\code{\link{DAxn}}.
}
\examples{
	#assumes SOA example life table to be load
	data(soaLt)
	soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
	x=x,lx=Ix,name="SOA2008"))
	out<-rLifeContingencies(n=1000, lifecontingency="Axn",object=soa08Act,
	x=40,t=getOmega(soa08Act)-40, i=soa08Act@interest,m=0)
	APV=Axn(soa08Act,x=40)
	#check if out distribution is unbiased
	t.test(x=out, mu=APV)$p.value>0.05
}

