\name{pxt}
\alias{dxt}
\alias{pxt}
\alias{qxt}
\title{Functions to evaluate survival, death probabilities and deaths.}
\description{These functions evaluate raw survival and death probabilities between age x and x+t}
\usage{
dxt(object, x, t, decrement)
pxt(object, x, t, fractional = "linear", decrement)
qxt(object, x, t, fractional = "linear", decrement)
}
\arguments{
  \item{object}{A \code{lifetable} object.}
  \item{x}{Age of life \code{x}.}
  \item{t}{Period until which the age shall be evaluated. Default value is 1.}
  \item{fractional}{Assumptions for fractional age. One of \code{"linear"}, 
  \code{"hyperbolic"}, \code{"constant force"} (can be abbreviated).}
  \item{decrement}{The reason of decrement (only for \code{mdt} class objects). Can be either an ordinal number or the
  name of decrement}
 }
\details{
	Fractional assumptions are:
	\itemize{
		\item linear: linear interpolation between consecutive ages, i.e. assume uniform distribution.
		\item constant force of mortality : constant force of mortality, also known as exponential interpolation.
		\item hyperbolic: Balducci assumption, also known as harmonic interpolation. 
		}
	Note that \code{fractional="uniform"}, \code{"exponential"}, \code{"harmonic"} or \code{"Balducci"}
	is also authorized.
	See references for details.
}
\value{A numeric value representing requested probability.}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}
\note{Function \code{dxt} accepts also fractional value of t. Linear interpolation is used in such case. These functions are called by many other functions.}

\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}
\seealso{\code{\link{exn}}, \code{\linkS4class{lifetable}}}
\examples{
	#dxt example
	data(soa08Act)
	dxt(object=soa08Act, x=90, t=2)
	#qxt example
	qxt(object=soa08Act, x=90, t=2)
	#pxt example
	pxt(object=soa08Act, x=90, t=2, "constant force" )
	#add another example for MDT
}

