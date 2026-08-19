\name{decreasingAnnuity}
\alias{decreasingAnnuity}
\title{Function to evaluate decreasing annuities.}
\description{This function returns present values for decreasing annuities-certain.}
\usage{
decreasingAnnuity(i, n, type = "immediate")
}
\arguments{
\item{i}{A numeric value representing the interest rate.}
\item{n}{The number of periods.}
\item{type}{The payment type. Use \code{"immediate"} (default) for an annuity-immediate, where payments are made at the end of each period, or \code{"due"} for an annuity-due, where payments are made at the beginning of each period. For compatibility, \code{"arrears"} is an alias for \code{"immediate"} and \code{"advance"} is an alias for \code{"due"} (can be abbreviated).}
}
\details{A decreasing annuity has the following flows of payments: n, n-1, n-2, \ldots, 1, 0. For an annuity-immediate these payments occur at times \eqn{1,2,\ldots,n}; for an annuity-due they occur at times \eqn{0,1,\ldots,n-1}.}
\value{A numeric value reporting the present value of the decreasing cash flows.}
\references{Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.}
\author{Giorgio A. Spedicato}
\note{This function calls \code{presentValue} function internally.}
\seealso{\code{\link{annuity}}, \code{\link{increasingAnnuity}}, \code{\link{DAxn}}}
\examples{
# The present value of 10, 9, 8, ..., 0 payable at the end of the period for 10 years is
decreasingAnnuity(i = 0.03, n = 10)
# Assuming a 3\% interest rate
sum((10:1)/(1 + .03)^(1:10))
}
\section{Warning}{The function is provided as is, without any guarantee regarding the accuracy of calculation. The author disclaims any liability for eventual losses arising from direct or indirect use of this software.}
