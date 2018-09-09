#############################################################################
#   Copyright (c) 2018 Christophe Dutang
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the
#   Free Software Foundation, Inc.,
#   59 Temple Place, Suite 330, Boston, MA 02111-1307, USA
#
#############################################################################
###
###         test argument functions
###


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


