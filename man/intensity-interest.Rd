\name{intensity2Interest}
\alias{intensity2Interest}
\alias{interest2Intensity}
\title{
Functions to switch from interest to intensity and vice versa.
}
\description{
There functions switch from interest to intensity and vice - versa.
}
\usage{

intensity2Interest(intensity)

interest2Intensity(i)
}

\arguments{

  \item{intensity}{Intensity rate}

  \item{i}{Interest rate}
}
\details{Simple financial mathematics formulas are applied.}
\value{A numeric value.}
\references{
	Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
Giorgio Alfredo Spedicato
}
\note{
Simple formulas are used
}
\seealso{
\code{\link{real2Nominal}}, \code{\link{nominal2Real}} 
}
\examples{
#an interest rate equal to 0.02 corresponds to a force of interest of of 
interest2Intensity(i=0.02)
#a force of interest of 0.02 corresponds to an APR of 
intensity2Interest(intensity=0.02)
}

