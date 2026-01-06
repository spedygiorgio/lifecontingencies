\name{getDecrements}
\alias{getDecrements}
\title{
Function to return the decrements defined in the mdt class
}
\description{
This function list the character decrements of the mdf class
}
\usage{
getDecrements(object)
}
\arguments{
  \item{object}{A \code{mdt} class object}
}
\details{A character vector is returned}
\value{A character vector listing the decrements defined in the class}
\references{
Marcel Finan A Reading of the Theory of Life Contingency 
Models: A Preparation for Exam MLC/3L
}
\author{Giorgio Alfredo Spedicato}
\note{
To be updated
}
\seealso{\code{\link{getOmega}}}
\examples{
#create a new table
tableDecr=data.frame(d1=c(150,160,160),d2=c(50,75,85))
newMdt<-new("mdt",name="testMDT",table=tableDecr)
getDecrements(newMdt)
}
