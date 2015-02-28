\name{interest2Discount}
\alias{interest2Discount}
\alias{discount2Interest}
\title{Functions to switch from interest to discount rates}
\description{
These functions switch from interest to discount rates and vice - versa}
\usage{

interest2Discount(i)

discount2Interest(d)
}
\arguments{

  \item{i}{
Interest rate}

  \item{d}{
Discount rate
}
}
\details{
The following formula (and its inverse) rules the relationships:
\deqn{\frac{i}{{1 + i}} = d}

}
\value{
A numeric value}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
Giorgio Alfredo Spedicato
}
\seealso{
\code{\link{intensity2Interest}},\code{\link{nominal2Real}}
}
\examples{
discount2Interest(d=0.04)
}