\name{increasingAnnuity}
\alias{increasingAnnuity}

\title{
\tIncreasing annuity.
}
\description{
\tThis function evaluates non - stochastic increasing annuities.
}
\usage{
\tincreasingAnnuity(i, n, type = "immediate")
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
\tAn increasing annuity shows the following flow of payments: \eqn{1,2,\ldots,n-1,n}.
\tFor an annuity-immediate these payments occur at times \eqn{1,2,\ldots,n};
\tfor an annuity-due they occur at times \eqn{0,1,\ldots,n-1}.
}
\value{
\tThe value of the annuity.
}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
\t2008, ACTEX Publications.
}
\author{
\tGiorgio A. Spedicato
}
\note{
\tThis function calls internally \code{presentValue} function.
}

\section{Warning }{
\tThe function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
\tlosses arising from direct or indirect use of this software.
}

\seealso{
\code{\link{decreasingAnnuity}},\code{\link{IAxn}}
}
\examples{
\t#the present value of 1,2,...,n-1, n sequence of payments, 
\t#payable at the end of the period
\t#for 10 periods is
\tincreasingAnnuity(i=0.03, n=10)
\t#assuming a 3% interest rate
}

