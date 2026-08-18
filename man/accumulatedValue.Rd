\name{accumulatedValue}
\alias{accumulatedValue}

\title{
\tFunction to evaluate the accumulated value.
}
\description{
\tThis functions returns the value at time n of a series of equally spaced payments of 1.
}
\usage{
\taccumulatedValue(i, n,m=0, k,type = "immediate")
}

\arguments{
\item{i}{
\tEffective interest rate expressed in decimal form. E.g. 0.03 means 3\%.
}
\item{n}{
\tNumber of terms of payment.
}
\item{m}{
\tDeferring period, whose default value is zero.
}
\item{k}{
\tFrequency of payment.
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
\tThe accumulated value is the future value of the terms of an annuity. Its mathematical expression is \eqn{s_{\left. {\overline {\, n \,}}\! \right| } = 
\t\left( {1 + i} \right)^n a_{\left. {\overline {\, n \,}}\! \right| }}
\tThe payment timing follows the \code{type} argument: an annuity-immediate
\thas payments at the end of each period, while an annuity-due has payments
\tat the beginning of each period.
}
\value{
\tA numeric value representing the calculated accumulated value.
}
\references{
\tBroverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
\t2008, ACTEX Publications.
}
\author{
\tGiorgio A. Spedicato
}
\note{
\tAccumulated value are derived from annuities by the following basic 
\tequation \eqn{{s_{\left. {\overline {\, 
 n \,}}\! \right| }} = {\left( {1 + i} \right)^n} = a_{\left. {\overline {\, 
 n \,}}\! \right| }}.
}


\section{Warning }{
\tThe function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
\tlosses arising from direct or indirect use of this software.
}

\seealso{
\code{\link{annuity}}
}
\examples{
#A man wants to save 100,000 to pay for his sons
#education in 10 years time. An education fund requires the investors to
#deposit equal installments annually at the end of each year. If interest of
#0.075 is paid, how much does the man need to save each year in order to
#meet his target?
R=100000/accumulatedValue(i=0.075,n=10)

}


