\name{pxt}
\alias{dxt}
\alias{pxt}
\alias{qxt}
\alias{dxt-methods}
\alias{pxt-methods}
\alias{qxt-methods}
\alias{dxt,ANY-method}
\alias{dxt,lifetable-method}
\alias{dxt,mdt-method}
\alias{pxt,ANY-method}
\alias{pxt,lifetable-method}
\alias{pxt,mdt-method}
\alias{qxt,ANY-method}
\alias{qxt,lifetable-method}
\alias{qxt,mdt-method}
\title{Functions to evaluate survival, death probabilities and deaths.}
\description{These functions evaluate raw survival and death probabilities between age x and x+t}
\usage{
dxt(object, x, t, decrement)
pxt(object, x, t, fractional = "linear", decrement)
qxt(object, x, t, fractional = "linear", decrement)
}
\arguments{
  \item{object}{A \code{lifetable}, \code{actuarialtable} or \code{mdt} object.}
  \item{x}{Age of life \code{x}. (can be a vector for \code{pxt, qxt}).}
  \item{t}{Period until which the age shall be evaluated. Default value is 1.
  (can be a vector for \code{pxt, qxt}).}
  \item{fractional}{Assumptions for fractional age. One of \code{"linear"}, 
  \code{"hyperbolic"}, \code{"constant force"} (can be abbreviated).}
  \item{decrement}{The reason of decrement (only for \code{mdt} class objects). Can be either ordinal numbers or the
  names of decrements; when several are given the probability (or number) of leaving the table because of any of them
  is returned.}
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

	\code{dxt}, \code{pxt} and \code{qxt} are S4 generics with methods for \code{lifetable} (and hence
	\code{actuarialtable}) and \code{mdt} objects; other packages can add methods for new table classes.

	For \code{mdt} objects with a \code{decrement}, \eqn{{}_tq_x^{(j)} = {}_td_x^{(j)} / l_x^{(\tau)}} and
	\code{pxt} returns its complement \eqn{1 - {}_tq_x^{(j)}}; ages must be tabulated integers and fractional
	\code{t} is interpolated linearly (UDD) within the year.
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
	#MDT example (Valdez)
	valdezMdt <- new("mdt", name = "ValdezExample", table = data.frame(
	  x = 50:54, lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
	  heart = c(5168, 5363, 5618, 5929, 6277),
	  accidents = c(1157, 1206, 1443, 1679, 2152),
	  other = c(4293, 5162, 5960, 6840, 7631)))
	qxt(valdezMdt, x = 50, t = 3, decrement = "heart")
	qxt(valdezMdt, x = 50, t = 3, decrement = c("heart", "accidents"))
	pxt(valdezMdt, x = 50, t = 3)
}

