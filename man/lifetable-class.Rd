\name{lifetable-class}
\Rdversion{1.1}
\docType{class}
\alias{lifetable-class}
\alias{coerce,data.frame,lifetable-method}
\alias{coerce,lifetable,data.frame-method}
\alias{coerce,lifetable,numeric-method}
\alias{coerce,lifetable,markovchainList-method}
\alias{getOmega,lifetable-method}
\alias{head,lifetable-method}
\alias{plot,lifetable,ANY-method}
\alias{print,lifetable-method}
\alias{show,lifetable-method}
\alias{summary,lifetable-method}
\alias{tail,lifetable-method}
\title{Class \code{"lifetable"}}
\description{
	\code{lifetable} objects allow to define and use life tables with the aim to evaluate 
	survival probabilities and mortality rates easily.
	Such values represent the building blocks used to estimate life insurances actuarial mathematics.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("lifetable", ...)}.
Two vectors are needed. The age vector and the population at risk vector.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"numeric"}, representing the sequence 0,1,\eqn{\ldots, \omega}}
    \item{\code{lx}:}{Object of class \code{"numeric"}, representing  the number of lives at the beginning of age \eqn{x}. It is a non 
    increasing sequence. The last element of vector x is supposed to be > 0.}
    \item{\code{name}:}{Object of class \code{"character"}, reporting the name of the table }
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "lifetable", to = "data.frame")}: method to create a data - frame from a lifetable object }
     \item{coerce}{\code{signature(from = "lifetable", to = "markovchainList")}: coerce method from \code{lifetable} to \code{markovchainList} }
     \item{coerce}{\code{signature(from = "lifetable", to = "numeric")}: brings to numeric }
    \item{coerce}{\code{signature(from = "data.frame", to = "lifetable")}: brings to life table }
      \item{getOmega}{\code{signature(object = "lifetable")}: returns the maximum attainable life age }
    \item{plot}{\code{signature(x = "lifetable", y = "ANY")}: plot method}
   \item{head}{\code{signature(x = "lifetable")}: head method }
    \item{print}{\code{signature(x = "lifetable")}: method to print the survival probability implied in the table }
    \item{show}{\code{signature(object = "lifetable")}: identical to \code{plot} method }
    \item{summary}{\code{signature(object = "lifetable")}: it returns summary information about the object }
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
	\code{t} may be missing in \code{pxt}, \code{qxt}, \code{ext}. It assumes value equal to 1 in such case.
}

\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\linkS4class{actuarialtable}}
}
\examples{
showClass("lifetable")
data(soa08)
summary(soa08)
#the last attainable age under SOA life table is
getOmega(soa08) 
#head and tail
data(soaLt)
tail(soaLt)
head(soaLt)
}
\keyword{classes}
