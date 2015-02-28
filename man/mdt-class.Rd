\name{mdt-class}
\Rdversion{1.1}
\docType{class}
\alias{mdt-class}
\alias{getDecrements,mdt-method}
\alias{getOmega,mdt-method}
\alias{initialize,mdt-method}
\alias{print,mdt-method}
\alias{coerce,mdt,data.frame-method}
\alias{coerce,mdt,markovchainList-method}
\alias{show,mdt-method}
\alias{summary,mdt-method}
\title{Class \code{"mdt"}}
\description{
A class to store multiple decrement tables
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("mdt", name, table, ...)}.
They store absolute decrements
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{The name of the table}
    \item{\code{table}:}{A data frame containing at least the number of decrements }
  }
}
\section{Methods}{
  \describe{
    \item{getDecrements}{\code{signature(object = "mdt")}: return the name of decrements }
    \item{getOmega}{\code{signature(object = "mdt")}: maximum attainable age }
    \item{initialize}{\code{signature(.Object = "mdt")}: method to initialize the class }
    \item{print}{\code{signature(x = "mdt")}: tabulate absolute decrement rates }
    \item{show}{\code{signature(object = "mdt")}: show rates of decrement }
    \item{coerce}{\code{signature(from = "mdt", to = "markovchainList")}: coercing to \code{markovchainList} objects }
    \item{coerce}{\code{signature(from = "mdt", to = "data.frame")}: coercing to \code{markovchainList} objects }
    \item{summary}{\code{signature(object = "mdt")}: it returns summary information about the object }
	 }
}
\references{
Marcel Finan A Reading of the Theory of Life Contingency 
Models: A Preparation for Exam MLC/3L
}
\author{
Giorgio Spedicato
}
\note{
Currently only decrements storage of the class is defined.
}
\seealso{\code{\linkS4class{lifetable}}}
\examples{#shows the class definition
showClass("mdt")
#create a new table
tableDecr=data.frame(d1=c(150,160,160),d2=c(50,75,85))
newMdt<-new("mdt",name="testMDT",table=tableDecr)
}
\keyword{classes}
