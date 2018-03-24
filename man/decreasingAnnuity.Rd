\name{decreasingAnnuity}
\alias{decreasingAnnuity}

\title{
	Function to evaluate decreasing annuities.
}
\description{
	This function return present values for decreasing annuities - certain.
}
\usage{
decreasingAnnuity(i, n,type="immediate")
}

\arguments{
  \item{i}{
	A numeric value representing the interest rate.
}
  \item{n}{
	The number of periods.
}
  \item{type}{
The Payment type, either \code{"advance"} for the annuity due (default)
or \code{"arrears"} for the annuity immediate.
Alternatively, one can use \code{"due"} or \code{"immediate"}
	respectively (can be abbreviated).
}
}
\details{
	A decreasing annuity has the following flows of payments: n, n-1, n-2, \ldots, 1, 0.
}
\value{
	A numeric value reporting the present value of the decreasing cash flows.
}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
	Giorgio A. Spedicato
}
\note{
	This function calls \code{presentValue} function internally.
}

\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\link{annuity}},\code{\link{increasingAnnuity}},\code{\link{DAxn}}
}
\examples{
	#the present value of 10, 9, 8,....,0 payable at the end of the period
	#for 10 years is
	decreasingAnnuity(i=0.03, n=10)
	#assuming a 3% interest rate
	#should be
	sum((10:1)/(1+.03)^(1:10))
}

