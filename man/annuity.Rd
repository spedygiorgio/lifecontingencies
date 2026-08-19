\name{annuity}
\alias{annuity}
\title{Annuity}
\description{This function calculates the present value of a stream of fixed payments separated by equal intervals of time.}
\usage{
annuity(i, n, m = 0, k = 1, type = "immediate")
}
\arguments{
\item{i}{Effective interest rate expressed in decimal form. E.g. 0.03 means 3\%. It can be a vector of interest rates of the same length of periods.}
\item{n}{Periods for payments. If n = \code{infinity} then \code{annuity} returns the value of a perpetuity (either immediate or due).}
\item{m}{Deferring period, whose default value is zero.}
\item{k}{Yearly payments frequency. A payment of \eqn{k^-1} is supposed to be performed at the end of each year.}
\item{type}{The payment type. Use \code{"immediate"} (default) for an annuity-immediate, where payments are made at the end of each period, or \code{"due"} for an annuity-due, where payments are made at the beginning of each period. For compatibility, \code{"arrears"} is an alias for \code{"immediate"} and \code{"advance"} is an alias for \code{"due"} (can be abbreviated).}
}
\details{For an annuity-immediate the first payment occurs at time \eqn{1/k}; for an annuity-due the first payment occurs at time \eqn{0}. Thus, for annual payments, an annuity-immediate has payment times \eqn{1,2,\ldots,n}, whereas an annuity-due has payment times \eqn{0,1,\ldots,n-1}.}
\value{A numeric value representing the present value of the annuity.}
\references{Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.}
\author{Giorgio A. Spedicato}
\note{The value returned by annuity function derives from direct calculation of the discounted cash flow and not from formulas, like \eqn{{a^{\left( m \right)}}_{\left. {\overline {\, n \,}}\! \right| } = \frac{{1 - {v^n}}}{{{i^{\left( m \right)}}}}}. When m is greater than 1, the payment per period is assumed to be \eqn{\frac{1}{m}}.}
