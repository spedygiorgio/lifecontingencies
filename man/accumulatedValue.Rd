\name{accumulatedValue}
\alias{accumulatedValue}
\title{Function to evaluate the accumulated value.}
\description{This function returns the value at time n of a series of equally spaced payments of 1.}
\usage{
accumulatedValue(i, n, m = 0, k = 1, type = "immediate")
}
\arguments{
\item{i}{Effective interest rate expressed in decimal form. E.g. 0.03 means 3\%.}
\item{n}{Number of terms of payment.}
\item{m}{Deferring period, whose default value is zero.}
\item{k}{Frequency of payment.}
\item{type}{The payment type. Use \code{"immediate"} (default) for an annuity-immediate, where payments are made at the end of each period, or \code{"due"} for an annuity-due, where payments are made at the beginning of each period. For compatibility, \code{"arrears"} is an alias for \code{"immediate"} and \code{"advance"} is an alias for \code{"due"} (can be abbreviated).}
}
\details{
The accumulated value is the future value of the terms of an annuity. Its mathematical expression is \eqn{s_{\left. {\overline {\, n \,}}\! \right| } = {\left( {1 + i} \right)^n} a_{\left. {\overline {\, n \,}}\! \right| }}.
The payment timing follows the \code{type} argument: an annuity-immediate has payments at the end of each period, while an annuity-due has payments at the beginning of each period.
}
\value{A numeric value representing the calculated accumulated value.}
\references{Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.}
\author{Giorgio A. Spedicato}
\note{Accumulated values are derived from annuities by the following basic equation \eqn{{s_{\left. {\overline {\, n \,}}\! \right| }} = {\left( {1 + i} \right)^n} = a_{\left. {\overline {\, n \,}}\! \right| }}.}
\section{Warning}{The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual losses arising from direct or indirect use of this software.}
\seealso{\code{\link{annuity}}}
