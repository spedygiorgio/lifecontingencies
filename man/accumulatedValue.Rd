\name{accumulatedValue}
\alias{accumulatedValue}

\title{
	Function to evaluate the accumulated value.
}
\description{
	This functions returns the value at time n of a series of equally spaced payments of 1.
}
\usage{
	accumulatedValue(i, n,m=0, k,type = "immediate")
}

\arguments{
\item{i}{
	Effective interest rate expressed in decimal form. E.g. 0.03 means 3\%.
}
\item{n}{
	Number of terms of payment.
}
\item{m}{
	Deferring period, whose default value is zero.
}
\item{k}{
	Frequency of payment.
}
\item{type}{
	A string, either "immediate" or "due".
}
}
\details{
	The accumulated value is the future value of the terms of an annuity. Its mathematical expression is \eqn{s_{\left. {\overline {\, n \,}}\! \right| } = 
	\left( {1 + i} \right)^n a_{\left. {\overline {\, n \,}}\! \right| }}
}
\value{
	A numeric value representing the calculated accumulated value.
}
\references{
	Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
	Giorgio A. Spedicato
}
\note{
	Accumulated value are derived from annuities by the following basic 
	equation \eqn{{s_{\left. {\overline {\, 
 n \,}}\! \right| }} = {\left( {1 + i} \right)^n} = a_{\left. {\overline {\, 
 n \,}}\! \right| }}.
}


\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.
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


