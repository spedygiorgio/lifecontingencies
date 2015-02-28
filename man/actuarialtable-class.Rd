\name{actuarialtable-class}
\Rdversion{1.1}
\docType{class}
\alias{actuarialtable-class}
\alias{coerce,actuarialtable,data.frame-method}
\alias{coerce,actuarialtable,numeric-method}
\alias{getOmega,actuarialtable-method}
\alias{print,actuarialtable-method}
\alias{show,actuarialtable-method}
\alias{summary,actuarialtable-method}
\title{Class \code{"actuarialtable"}}
\description{
Objects of class \code{"actuarialtable"} inherit the structure of class \code{"lifetable"} adding just the slot for interest rate, \code{interest}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("actuarialtable", ...)}.
Creation is the same as lifetable objects creation, the slot for interest must be added too.
}
\section{Slots}{
  \describe{
    \item{\code{interest}:}{Object of class \code{"numeric"} slot for interest rate, e.g. 0.03}
    \item{\code{x}:}{Object of class \code{"numeric"} age slot}
    \item{\code{lx}:}{Object of class \code{"numeric"} subjects at risk at age x}
    \item{\code{name}:}{Object of class \code{"character"} name of the actuarial table}
  }
}
\section{Extends}{
Class \code{"\linkS4class{lifetable}"}, directly.
}
\section{Methods}{
  \describe{
  \item{coerce}{\code{signature(from = "actuarialtable", to = "data.frame")}: moves from \code{actuarialtable} to \code{data.frame} }
    \item{coerce}{\code{signature(from = "actuarialtable", to = "numeric")}: coerce from \code{actuarialtable} to a numeric }
    \item{getOmega}{\code{signature(object = "actuarialtable")}: as for \code{lifetable} }
    \item{print}{\code{signature(x = "actuarialtable")}: tabulates the actuarial commutation functions }
    \item{show}{\code{signature(object = "actuarialtable")}: show method }
    \item{summary}{\code{signature(object = "actuarialtable")}: prints brief summary }
	 }
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato
}
\note{
The interest slot will handle time-varying interest rates in the future.
}

\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}
\seealso{
\code{\link{axn}},\code{\linkS4class{lifetable}}
}
\examples{
showClass("actuarialtable")
}
\keyword{classes}
