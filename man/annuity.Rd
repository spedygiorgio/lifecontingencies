\name{annuity}
\alias{annuity}
\title{
Annuity function
}
\description{
Function to calculate present value of annuities-certain.
}
\usage{
annuity(i, n,m=0, k=1,type = "immediate")
}

\arguments{
\item{i}{
	Effective interest rate expressed in decimal form. E.g. 0.03 means 3\%. It can be a vector of interest rates of the same length
	of periods. 
}
\item{n}{
	Periods for payments. If n = \code{infinity} then \code{annuity} returns the value of a perpetuity (either immediate or due). 
}
\item{m}{
	Deferring period, whose default value is zero.
}
\item{k}{
	Yearly payments frequency. A payment of \eqn{k^-1} is 
	supposed to be performed at the end of each year.
}
\item{type}{
	A string, either "immediate" or "due".
}
}
\details{
	This function calculates the present value of a stream of fixed payments 
	separated by equal interval of time. Annuity immediate has the 
	fist payment at time t=0, while an annuity due has the first payment at time t=1.
}
\value{
	A string, either "immediate" or "due".
}
\references{
	Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
	Giorgio A. Spedicato
}
\note{
	The value returned by annuity function derives from direct calculation of the discounted cash flow and 
	not from formulas, like \eqn{{a^{\left( m \right)}}_{\left. {\overline {\, 
 n \,}}\! \right| } = \frac{{1 - {v^n}}}{{{i^{\left( m \right)}}}}}. When m is greater than 
 1, the payment per period is assumed to be \eqn{\frac{1}{m}}.
}



\seealso{
\code{\link{accumulatedValue}}
}
\examples{
# The present value of 5 payments of 1000 at one year interval that begins 
# now when the interest rate is 2.5% is
1000*annuity(i=0.05, n=5, type = "due")
#A man borrows a loan of 20,000 to purchase a car at
# a nominal annual rate of interest of 0.06. He will pay back the loan through monthly
#installments over 5 years, with the first installment to be made one month
#after the release of the loan. What is the monthly installment he needs to pay?
R=20000/annuity(i=0.06/12, n=5*12)
}

