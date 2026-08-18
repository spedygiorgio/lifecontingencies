\name{decreasingAnnuity}
\alias{decreasingAnnuity}

\title{
\tFunction to evaluate decreasing annuities.
}
\description{
\tThis function return present values for decreasing annuities - certain.
}
\usage{
decreasingAnnuity(i, n,type="immediate")
}

\arguments{
  \item{i}{
\tA numeric value representing the interest rate.
}
  \item{n}{
\tThe number of periods.
}
  \item{type}{
The payment type. Use \code{"immediate"} (default) for an annuity-immediate,
where payments are made at the end of each period, or \code{"due"} for an
annuity-due, where payments are made at the beginning of each period.
For compatibility, \code{"arrears"} is an alias for \code{"immediate"} and
\code{"advance"} is an alias for \code{"due"} (can be abbreviated).
}
}
\details{
\tA decreasing annuity has the following flows of payments: n, n-1, n-2, \ldots, 1, 0.
\tFor an annuity-immediate these payments occur at times \eqn{1,2,\ldots,n};
\tfor an annuity-due they occur at times \eqn{0,1,\ldots,n-1}.
}
\value{
\tA numeric value reporting the present value of the decreasing cash flows.
}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
\t2008, ACTEX Publications.
}
\author{
\tGiorgio A. Spedicato
}
\note{
\tThis function calls \code{presentValue} function internally.
}

\section{Warning }{
\tThe function is provided as is, without any guarantee regarding the accuracy of calculation. The author disclaims any liability for eventual 
\tlosses arising from direct or indirect use of this software.
}

\seealso{
\code{\link{annuity}},\code{\link{increasingAnnuity}},\code{\link{DAxn}}
}
\examples{
\t#the present value of 10, 9, 8,....,0 payable at the end of the period
\t#for 10 years is
\tdecreasingAnnuity(i=0.03, n=10)
\t#assuming a 3% interest rate
\t#should be
\tsum((10:1)/(1+.03)^(1:10))
}

