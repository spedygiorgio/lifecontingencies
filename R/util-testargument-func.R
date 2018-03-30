#
# Author: Christophe Dutang 
###############################################################################

#non exported functions


# fractional argument
#see page 74 of Actuarial Mathematics
testfractionnalarg <- function(x)
{
  x <- match.arg(x, c("linear", "uniform", "hyperbolic", "constant force", "exponential", "harmonic", "Balducci"))
  if(x == "uniform")
    x <- "linear"
  if(x == "exponential")
    x <- "constant force"
  if(x %in% c("harmonic", "Balducci"))
    x <- "hyperbolic"
  x
}
#multiple-life status 
teststatusarg <- function(x)
{
  x <- match.arg(x, c("last", "joint", "Joint-Life", "Last-Survivor"))
  if(x == "Joint-Life")
    x <- "joint"
  if(x == "Last-Survivor")
    x <- "last"
  x
}


#random life type variable
testtypelifearg <- function(x)
{
  x <- match.arg(x, c("Tx", "Kx", "continuous", "curtate", "complete"))
  if(x %in% c("continuous", "complete"))
    x <- "Tx"
  if(x == "curtate")
    x <- "Kx"
  x
}

#output type
testtyperesarg <- function(x)
{
  x <- match.arg(x, c("EV", "ST", "expected", "stochastic"))
  if(x == "expected")
    x <- "EV"
  if(x == "stochastic")
    x <- "ST"
  x
}
#payment argument for annuities
testpaymentarg <- function(x)
{
  x <- match.arg(x, c("advance", "due", "immediate", "arrears"))
  if(x == "advance")
    x <- "due"
  if(x == "arrears")
    x <- "immediate"
  x
}



#life computation type
testlifecontarg <- function(x)
{
  x <- match.arg(x, c("Axn", "axn", "Exn", "IAxn", "DAxn", "AExn"))
  x
}
testlifecontarg2 <- function(x)
{
  x <- match.arg(x, c("Axyz", "axyz"))
  x
}


