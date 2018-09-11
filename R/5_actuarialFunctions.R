#############################################################################
#   Copyright (c) 2018 Giorgio A. Spedicato, Christophe Dutang
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
###         actuarial functions
###




#' Function to evaluate the pure endowment
#'
#' @param actuarialtable An actuarial table object.
#' @param x Age of the insured.
#' @param n Length of the contract.
#' @param i Interest rate (it overwrites the \code{actuarialtable} one)
#' @param type A string, eithed "EV" (default value),  "ST" (stocastic realization) or "VR" if the value of the variance is needed.
#' @param power The power of the APV. Default is 1 (mean)
#'
#' @return The APV of the contract
#' @seealso \code{\link{axn}}
#' @author Giorgio A. Spedicato
#' @references Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., 
#' Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
#'
#' @examples
#' 
#' #assumes SOA example life table to be load
#' data(soaLt)
#' soa08Act=with(soaLt, new("actuarialtable",interest=0.06, x=x,lx=Ix,name="SOA2008"))
#' #evaluate the pure endowment for a man aged 30 for a time span of 35
#' Exn(soa08Act, x=30, n=35) 
#' @rdname pureendowment
#' @export
Exn <- function(actuarialtable, x, n, i = actuarialtable@interest, type = "EV", power = 1)
  {
    interest <- i
    out <- numeric(1)
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable") #request an actuarial actuarialtable
    type <- testtyperesarg(type)
    prob = pxt(actuarialtable,x,n)
    discount = (1 + interest) ^ (-n)
    #defines the outputs
    if (type == "EV")
      out = presentValue(
        cashFlows = 1, timeIds = n,
        interestRates = interest, probabilities = prob,power = power
      )
    else if (type == "ST")
      out = rLifeContingencies(
        n = 1,lifecontingency = "Exn",
        object = actuarialtable, x = x,t = n,i = interest, m = 1,k = 1
      )
    #out=discount^2*prob*(1-prob)
    return(out)
}


#function computing survival annuities
axn <- function(actuarialtable, x, n, i = actuarialtable@interest, m,
                    k = 1, type = "EV",power = 1, payment = "advance", ...)
{
  #checks
  if (!(class(actuarialtable) %in% c("lifetable","actuarialtable")))
    stop("Error! Only lifetable, actuarialtable classes are accepted")
  
  type <- testtyperesarg(type)
  payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
  
  #class(object) %in% c("lifetable","actuarialtable") 
  if (missing(x))
    stop("Missing x")
  if(length(k) > 1)
  {
    k <- k[1]
    warnings("k should be of length 1, it takes first value")
  }
  if (missing(m))
    m <- 0
  if (missing(n))
    n <- ceiling((getOmega(actuarialtable) + 1 - x - m) * k) / k 
  if (any(x < 0, n < 0, m < 0))
    stop("Check x, n or m")
  
  if(length(x) <= 0)
    stop("x is of length zero")
  if(length(t) <= 0)
    stop("t is of length zero")
  
  ntot <- max(length(n), length(x), length(m))
  if(length(n) != length(x) || length(n) != length(m))
  {
    if(length(m) != ntot)
      warnings("m argument has been recycled to match the maximum length of x, m and n")
    if(length(n) != ntot)
      warnings("n argument has been recycled to match the maximum length of x, m and n")
    if(length(x) != ntot)
      warnings("x argument has been recycled to match the maximum length of x, m and n")
    m <- rep(m, length.out=ntot)
    n <- rep(n, length.out=ntot)
    x <- rep(x, length.out=ntot)
    if(any(is.infinite(x), is.infinite(n), is.infinite(m)))
      stop("infinite values provided in x, n or m")
    if(any(x < 0, n < 0, m < 0))
      stop("(strictly) negative values provided in x, n or m")
  }
  
  
  if (type == "EV")
  {
    single_axn_immediate <- function(j)
    {  
      if(n[j] <= 0)
        return(0)
      #computation of quantities, assuming fractional payments
      payments <- rep(1 / k, n[j] * k)
      times <- m[j] + seq(from = 1/k, to = n[j], by = 1/k)
      probs <- pxt(actuarialtable, x[j], times, ...)
      #pxtvect(object, x, t, fractional = "linear", decrement)
      presentValue(payments, times, i, probs, power)
    }
    single_axn_due <- function(j)
    {  
      if(n[j] <= 0)
        return(0)
      #computation of quantities, assuming fractional payments
      payments <- rep(1 / k, n[j] * k)
      times <- m[j] + seq(from = 0, to = n[j]-1/k, by = 1/k)
      probs <- pxt(actuarialtable, x[j], times, ...)
      presentValue(payments, times, i, probs, power)
    }
    if(payment == "immediate")
      out <- sapply(1:ntot, single_axn_immediate)
    else if(payment == "due")
      out <- sapply(1:ntot, single_axn_due)
    else
      stop("wrong payment type")
  } else if (type == "ST") {
    
    rng_axn <- function(j)
      rLifeContingencies(
        n = 1, lifecontingency = "axn", object = actuarialtable, 
        x = x[j], t = n[j], i = i, m = m[j], k = k, payment=payment)
    out <- sapply(1:ntot, rng_axn)
  } else
    stop("wrong result type")
  out  
}

#function to obtain the annuity
axnold <- function(actuarialtable, x, n, i = actuarialtable@interest, m,
                k = 1, type = "EV", power = 1, payment = "advance")
  {
    interest <- i
    out <- numeric(1)
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable")
    type <- testtyperesarg(type)
    payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
    
    if (missing(x))
      stop("Error! Need age!")
    
    if (x > getOmega(actuarialtable)) {
      out = 0
      return(out)
    }
    if (missing(m))
      m = 0
    if (missing(n))
      n = ceiling((getOmega(actuarialtable) + 1 - x - m) * k) / k #n=getOmega(actuarialtable)-x-m Patch by Reinhold
    if (n == 0) {
      out = 0
      return(out)
    }
    if (any(x < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    #computation of quantities, assuming fractional payments
    payments = rep(1 / k,n * k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    if (payment == "immediate")
      times = times + 1 / k
    
    for (i in 1:length(times))
      probs[i] = pxtold(actuarialtable, x,times[i])
    discounts = (1 + interest) ^ -times #prima era asteriskato
    #out<-sum(payments*discounts*probs)
    if (type == "EV") {
      out <-
        presentValue(
          cashFlows = payments, timeIds = times, interestRates = interest, probabilities =
            probs,power = power
        )
      #out=.C("add3", x=as.double(payments), y=as.double(discounts),z=as.double(probs),n=as.integer(length(probs)),out=numeric(1))$out
    } else if (type == "ST") {
      out = rLifeContingencies(
        n = 1,lifecontingency = "axn",
        object = actuarialtable, x = x,t = n,i = interest, m = m,k = k, payment =
          payment
      )
    }
    return(out)
  }





#shall write the Rd file
axyn <- function(tablex, tabley, x,y, n,i, m,k = 1, status = "joint", type = "EV", payment =
             "advance")
  {
    .Deprecated("axyzn")
    out <- numeric(1)
    if (missing(tablex))
      stop("Error! Need table for X life")
    if (missing(tabley))
      stop("Error! Need table for Y life")
    if (missing(x))
      stop("Error! Need age for X!")
    if (missing(y))
      stop("Error! Need age for Y!")
    type <- testtyperesarg(type)
    payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
    status <- teststatusarg(status)
    
    if (missing(m))
      m = 0
    if (missing(n))
      n = max(getOmega(tablex) - x,getOmega(tabley) - y) - m #maximum sequence of payments
    if (tablex@interest != tabley@interest) {
      warning("Warning! Intesters differ between tablex and tabley. Using average")
    }
    if (!missing(i))
      interest = i
    else
      interest = 0.5 * (tablex@interest + tabley@interest) #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
    if (any(x < 0,y < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    #computation of quantities, assuming fractional payments
    payments = rep(1 / k,n * k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    if (payment == "immediate")
      times = times + 1 / k
    
    xVec = c(x,y)
    tablesList = list(tablex, tabley)
    
    for (i in 1:length(times))
      probs[i] = pxyzt(
        tablesList = tablesList,x = xVec,
        t = times[i],status = status
      )
    #discounts=(1+actuarialtable@interest)^-times
    #out<-sum(payments*discounts*probs)
    if (type == "EV") {
      out <-
        presentValue(
          cashFlows = payments, timeIds = times, interestRates = interest, probabilities =
            probs
        )
    } else	if (type == "ST") {
      out = 0
      for (i in 1:length(times))
        out = out + 1 / k * rbinom(n = 1, size = 1, prob = probs[i]) * (1 + interest) ^
          -times[i]
    }
    return(out)
  }


axyznvect <- function(tablesList, x, n, i, m, k = 1, status = "joint", type = "EV",
                      power =1, payment = "advance", ...)
{
  #initial checkings
  numTables = length(tablesList)
  if (missing(x))
    stop("Missing x")
  if(!is.numeric(x))
    stop("Error! x should be a numeric vector or a numeric matrix")
  else if(is.vector(x))
  {  
    if(length(x) == 1)
      x <- rep(x, length.out=numTables)
    if (length(x) != numTables)
      stop("Error! Initial ages vector length does not match with number of lives")
  }else if(is.matrix(x))
  {
    if(NCOL(x) != numTables)
      stop("Error! Age matrix colum number does not match with number of lives")
  }else
    stop("Error! x should be a numeric vector or a numeric matrix")
  
  if(is.vector(x))
    x <- t(as.matrix(x))
  #from here, x is a matrix where NCOL(x) == numTables
  nbcomput <- NROW(x)
  
  classlist <- sapply(tablesList, class)
  if(any(!classlist %in% c("lifetable", "actuarialtable")))
    stop("Error! A list of lifetable objects is required")
  
  type <- testtyperesarg(type)
  payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
  status <- teststatusarg(status)
  
  #class(object) %in% c("lifetable","actuarialtable") 
  if(length(k) > 1)
  {
    k <- k[1]
    warnings("k should be of length 1, it takes first value")
  }
  if (missing(m))
    m <- 0
  if (any(x < 0, m < 0))
    stop("Check x, n or m")
  
  if(length(x) <= 0)
    stop("x is of length zero")
  if(length(t) <= 0)
    stop("t is of length zero")
  
  missingn <- missing(n)
  if(!missingn)
  {
    if(length(n) <= 0)
      stop("n is of length zero")
    if(!is.numeric(n))
      stop("Error! n should be a numeric vector")
  }else
  {
    computn <- function(idrow)
    {
      getn <- function(j)
        ceiling((getOmega(tablesList[[j]]) + 1 - x[idrow, j] - m) * k) / k 
      myn <- max(sapply(1:numTables, getn))
    }
    n <- sapply(1:nbcomput, computn)
  }
  
  #recycle some parameters
  if(length(m) != nbcomput)
  {
    m <- rep(m, length.out = nbcomput)
    warnings("m argument has been recycled to match the row number of x")
  }
  if(length(k) != nbcomput)
  {
    k <- rep(k, length.out = nbcomput)
    warnings("k argument has been recycled to match the row number of x")
  }
  if(length(n) != nbcomput)
  {
    n <- rep(n, length.out = nbcomput)
    warnings("n argument has been recycled to match the row number of x")
  }
  
  
  if (!missing(i))
  {
    if(!is.numeric(i))
      stop("Error! i should be a numeric vector")
    interest = i
  }else 
  {
    interest <- sapply(1:numTables, function(j) tablesList[[j]]@interest)
  }
  if(length(interest) > 1)
  {
    interest <- mean(interest)
    warnings("i argument is the average value of interest values")
  }
  
  
  #computation of quantities, assuming fractional payments
  allpayments <- matrix(1 / k, nrow=nbcomput, ncol=n * k)
  
  computtime <- function(idrow)
  {
    if(payment == "immediate")
    {  
      times <- m[idrow] + seq(from = 1/k, to = n[idrow], by = 1/k)
    }else if(payment == "due")
    {  
      times <- m[idrow] + seq(from = 0, to = n[idrow]-1/k, by = 1/k)
    }
    else
      stop("wrong payment type")
    times
  }
  alltime <- t(sapply(1:nbcomput, computtime))
  
  computprob <- function(idrow)
  {
    #replicate time over lifetables
    valt <- replicate(numTables, alltime[idrow,])
    valx <- x[idrow,]
    pxyzt(tablesList, valx, valt, status=status, ...)
  }
  allprob <- t(sapply(1:nbcomput, computprob))
  if(NROW(allprob) != nbcomput)
    stop("wrong probabilities computed")
  
  res <- sapply(1:nbcomput, function(idrow)
    presentValue(allpayments[idrow,], alltime[idrow,], interest, allprob[idrow,], power))
  
  res
}

axyzn <- function(tablesList, x, n,i, m,k = 1, status = "joint", type = "EV",power =
             1, payment = "advance")
  {
    out <- numeric(1)
    #initial checkings
    numTables = length(tablesList)
    if (length(x) != numTables)
      stop("Error! Initial ages vector length does not match with number of lives")
    for (j in 1:numTables) {
      if (!(class(tablesList[[j]]) %in% c("lifetable", "actuarialtable")))
        stop("Error! A list of lifetable objects is required")
    }
    type <- testtyperesarg(type)
    payment <- testpaymentarg(payment) # "advance"->"due"; "arrears"->"immediate"
    status <- teststatusarg(status)
    
    if (k < 1)
      stop("Error! Periods in a year shall be no less than 1")
    if (missing(m))
      m = 0
    
    if (missing(n)) {
      n = 0
      for (j in 1:numTables)
        n = (max(n,(getOmega(tablesList[[j]]) - x[j])))
      n = ceiling((n+1 - m)*k)/k
      #n=n-m-1 patch by Reinhold
    }
    
    if (!missing(i))
      interest = i
    else {
      temp = 0
      for (j in 1:numTables)
        temp = temp + tablesList[[j]]@interest
      interest = temp / numTables
    }
    
    if (any(x < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    #computation of quantities, assuming fractional payments
    payments = rep(1 / k,n * k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    if (payment == "immediate")
      times = times + 1 / k
    for (j in 1:length(times))
      probs[j] = pxyzt(
        tablesList = tablesList,x = x,
        t = times[j],status = status
      )
    discounts = (1 + interest) ^ -times #prima asteriskato
    #out<-sum(payments*discounts*probs)
    if (type == "EV") {
      out <-
        presentValue(
          cashFlows = payments, timeIds = times, interestRates = interest, probabilities =
            probs,power = power
        )
      #out=.C("add3", x=as.double(payments), y=as.double(discounts),z=as.double(probs),n=as.integer(length(probs)),out=numeric(1))$out
    } else	if (type == "ST") {
      out = rLifeContingenciesXyz(
        n = 1,lifecontingency = "axyz", tablesList = tablesList, x = x,t = n,i =
          i, m = m,k = k, status = status, payment = payment
      )
    }
    return(out)
  }

#tablesList=list(soa08Act,soa08Act);x=c(67,65);n=20;k=11

# objectX=soa08Act
# objectY=soa08Act
# tx=65
# ty=63
# n=10

# aX=axn(soa08Act, x=tx, n=n)
# aY=axn(soa08Act, x=ty, n=n)
# axynJ=axyn(tablex=objectX,tabley=objectY, x=tx, y=ty,n=n, status="joint")
# axynL=axyn(objectX,objectY, x=tx,  y=ty,n=n, status="last", type="EV")
# axynL
# (aX+aY-axynJ)-axynL #minimum departure

#function to obtain the Life Insurance
#actuarialtable: an actuarial actuarialtable object
#x: beginnin life age
#m: deferring term
#type: output requested: default expected value
Axnold <- function(actuarialtable, x, n,i = actuarialtable@interest, m, k = 1, type =
             "EV",power = 1)
  {
    out <- numeric(1)
    interest <- i
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable")
    type <- testtyperesarg(type)
    
    if (missing(x))
      stop("Error! Need age!")
    if (k < 1)
      stop("Error! Periods in a year shall be no less than 1")
    if (missing(m))
      m <- 0
    if (missing(n))
      n <- ceiling((getOmega(actuarialtable)+1 - x - m) * k) / k  # want n to be a multiple of 1/k
#       n = getOmega(actuarialtable) - x - m + 1 #Rosa patch
    if (n == 0)
      return(0)
    if (any(x < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    #we have n*k possible payment times
    payments = rep(1,n * k) #the payment is fixed
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    startAge = x
    
    #for(i in 1:length(times)) probs[i]=(pxt(object=actuarialtable, x=startAge,t=times[i])*qxt(object=actuarialtable, x=startAge+times[i],t=1/k))
    for (i in 1:length(times))
      probs[i] = .qxnt(
        object = actuarialtable, x = startAge,n = times[i],t = 1 / k
      )
    discounts = (1 + interest) ^ -(times + 1 / k) #prima asteriskato
    
    if (type == "EV") {
      #out<-sum(payments*discounts*probs)
      out <-
        presentValue(
          cashFlows = payments, timeIds = (times + 1 / k), interestRates = interest, probabilities =
            probs,power = power
        )
      #out=.C("add3", x=as.double(payments), y=as.double(discounts),z=as.double(probs),n=as.integer(length(payments)),out=numeric(1))$out
    } else if (type == "ST") {
      out = rLifeContingencies(
        n = 1,lifecontingency = "Axn", object = actuarialtable, x = x,t = n,i =
          interest, m = m,k = k
      )
    }
    return(out)
  }


#function computing survival annuities

Axn <- function(actuarialtable, x, n, i = actuarialtable@interest, m,
                    k = 1, type = "EV", power = 1, ...)
{
  #checks
  if (!(class(actuarialtable) %in% c("lifetable","actuarialtable")))
    stop("Error! Only lifetable, actuarialtable classes are accepted")
  
  type <- testtyperesarg(type)
  
  #class(object) %in% c("lifetable","actuarialtable") 
  if (missing(x))
    stop("Missing x")
  if(length(k) > 1)
  {
    k <- k[1]
    warnings("k should be of length 1, it takes first value")
  }
  if (missing(m))
    m <- 0
  if (missing(n))
    n <- ceiling((getOmega(actuarialtable) + 1 - x - m) * k) / k 
  if (any(x < 0, n < 0, m < 0))
    stop("Check x, n or m")
  
  if(length(x) <= 0)
    stop("x is of length zero")
  if(length(t) <= 0)
    stop("t is of length zero")
  
  ntot <- max(length(n), length(x), length(m))
  if(length(n) != length(x) || length(n) != length(m))
  {
    if(length(m) != ntot)
      warnings("m argument has been recycled to match the maximum length of x, m and n")
    if(length(n) != ntot)
      warnings("n argument has been recycled to match the maximum length of x, m and n")
    if(length(x) != ntot)
      warnings("x argument has been recycled to match the maximum length of x, m and n")
    m <- rep(m, length.out=ntot)
    n <- rep(n, length.out=ntot)
    x <- rep(x, length.out=ntot)
    if(any(is.infinite(x), is.infinite(n), is.infinite(m)))
      stop("infinite values provided in x, n or m")
    if(any(x < 0, n < 0, m < 0))
      stop("(strictly) negative values provided in x, n or m")
  }
  
  
  if (type == "EV")
  {
    single_Axn <- function(j)
    {  
      if(n[j] <= 0)
        return(0)
      #the payment is fixed
      payments <- rep(1, n[j] * k)
      times <- m[j] + seq(from = 0, to = (n[j]-1/k), by = 1/k)
      probs <- pxt(actuarialtable, x=x[j], t=times, ...) * qxt(actuarialtable, x=x[j]+times, t=1/k, ...)
      presentValue(payments, timeIds=(times + 1/k), interestRates=i, 
                   probabilities=probs, power=power)
    }
    out <- sapply(1:ntot, single_Axn)
    
    
  } else if (type == "ST") {
    
    rng_Axn <- function(j)
      rLifeContingencies(
        n = 1, lifecontingency = "Axn", object = actuarialtable, 
        x = x[j], t = n[j], i = i, m = m[j], k = k)
    out <- sapply(1:ntot, rng_Axn)
  } else
    stop("wrong result type")
  out  
}

Axyn <- function(tablex, x,tabley, y, n,i, m, k = 1, status = "joint", type = "EV")
  {
    .Deprecated("Axyzn")
    out <- numeric(1)
    if (any(missing(tablex),missing(tabley)))
      stop("Error! Need tables")
    if (any(missing(x),missing(y)))
      stop("Error! Need ages!")
    type <- testtyperesarg(type)
    status <- teststatusarg(status)
    
    if (k < 1)
      stop("Error! Periods in a year shall be no less than 1")
    if (missing(m))
      m = 0
    if (missing(n))
      n = max(getOmega(tablex) - x,getOmega(tabley) - y) - m - 1
    
    if (tablex@interest != tabley@interest) {
      warning("Warning! Intesters differ between tablex and tabley. The average will be used")
    }
    if (!missing(i))
      interest = i
    else
      interest = 0.5 * (tablex@interest + tabley@interest) #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
    
    if (n == 0)
      return(0)
    if (any(x < 0,y < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    
    #perform calculations
    
    payments = rep(1,n * k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    startAgex = x
    startAgey = y
    tablesList = list(tablex, tabley)
    for (i in 1:length(times))
      probs[i] = .qxyznt(
        tablesList = tablesList,x = c(startAgex,startAgey),n = times[i],t = 1 /
          k, status = status
      )
    discounts = (1 + interest) ^ -(times + 1 / k)
    
    if (type == "EV") {
      out <- sum(payments * discounts * probs)
    } else if (type == "ST") {
      out = 0
      for (i in 1:length(times))
      {
        out = ((1 + interest) ^ -(times[i] + 1 / k)) * rbinom(n = 1, size = 1, prob =
                                                                probs[i])
        if (out > 0)
          break
      }
    }
    #warning("Warning: this function in deprecated. Use Axyzn instead!")
    return(out)
  }


Axyzn <- function(tablesList, x, n,i, m, k = 1, status = "joint", type = "EV",power =
             1)
  {
    out = numeric(1)
    #initial checkings
    numTables = length(tablesList)
    if (length(x) != numTables)
      stop("Error! Initial ages vector length does not match with number of lives")
    for (j in 1:numTables) {
      if (!(class(tablesList[[j]]) %in% c("lifetable", "actuarialtable")))
        stop("Error! A list of lifetable objects is required")
    }
    type <- testtyperesarg(type)
    status <- teststatusarg(status)
    
    if (k < 1)
      stop("Error! Periods in a year shall be no less than 1")
    if (missing(m))
      m = 0
    
    if (missing(n)) {
      n = 0
      for (j in 1:numTables)
        n = (max(n,(getOmega(tablesList[[j]]) - x[j])))
      n = n - m - 1
#       n = n - m + 1 #Rosa patch
    }
    
    if (!missing(i))
      interest = i
    else {
      temp = 0
      for (j in 1:numTables)
        temp = temp + tablesList[[j]]@interest
      interest = temp / numTables
    }
    
    if (n == 0)
      return(0)
    if (any(x < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    
    #perform calculations
    
    payments = rep(1,n * k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    
    
    for (j in 1:length(times))
      probs[j] = .qxyznt(
        tablesList = tablesList,x = x,n = times[j],t = 1 / k, status = status
      )
    discounts = (1 + interest) ^ -(times + 1 / k)
    
    if (type == "EV") {
      out <- sum(((payments * discounts) ^ power) * probs)
      #out=.C("add3", x=as.double(payments), y=as.double(discounts),z=as.double(probs),n=as.integer(length(payments)),out=numeric(1))$out
    } else if (type == "ST") {
      out = 0
      out = rLifeContingenciesXyz(
        n = 1,lifecontingency = "Axyz",
        tablesList = tablesList, x = x,t = n,i = i, m = m,k = k,status = status
      )
    }
    return(out)
  }

#Axyzn(tablesList, x, n, status="joint", type="ST")


# tablex=soa08Act
# tabley=soa08Act
# x=65
# y=63
# n=10
# i=0.06
# m=0
#
# Ax=Axn(soa08Act, x=x,n=n)
# Ay=Axn(soa08Act, x=y,n=n)
# AxynJ=Axyn(tablex=soa08Act, x=x,tabley=soa08Act, y=y, n=n,i=i, m=m, k=1, status="joint", type="EV")
# AxynL=Axyn(tablex=soa08Act, x=x,tabley=soa08Act, y=y, n=n,i=i, m=m, k=1, status="last", type="EV")
# AxynJ
# AxynL
#Ax+Ay-AxynJ


#n-year term whole life
#recursive function
# actuarialtable=soa08Act
# x=50
# n=10
# i=0.06
# m=0
# k=2

IAxn <- function(actuarialtable, x, n,i = actuarialtable@interest, m = 0, k = 1, type =
             "EV",power = 1)
  {
    out <- numeric(1)
    interest <- i
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable")
    if (missing(m))
      m = 0
    if (missing(x))
      stop("Error! Need age!")
    type <- testtyperesarg(type)
    
    if (missing(n))
      n = getOmega(actuarialtable) - x - m #n=getOmega(actuarialtable)-x-m-1
    
    if (any(x < 0,m < 0,n < 0))
      stop("Error! Negative parameters")
    #we have n*k possible payment times
    payments = seq(from = 1 / k, to = n, by = 1 / k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    startAge = x #we start from x
    for (i in 1:length(times))
      probs[i] = (
        pxt(object = actuarialtable, x = startAge,t = times[i]) * qxt(
          object = actuarialtable, x = startAge + times[i],t = 1 / k
        )
      )
    discounts = (1 + interest) ^ -(times + 1 / k)
    
    if (type == "EV") {
      #		payments=seq(from=1, to=n, by=1)
      #		probs=numeric(n)
      #		times=m+seq(from=0, to=(n-1),by=1)
      #		for(i in 1:length(times)) probs[i]=(pxt(actuarialtable, x,times[i])*qxt(actuarialtable,
      #								x+times[i],1))
      #		discounts=(1+interest)^-(times+1)
      out <- sum(((payments * discounts) ^ power) * probs)
    } else if (type == "ST") {
      out = rLifeContingencies(
        n = 1,lifecontingency = "IAxn", object = actuarialtable, x = x,t = n,i =
          interest, m = m,k = k
      ) #!fix: prima = 1
    }
    return(out)
  }

# n=500000
# lifecontingency="IAxn"
# object=soa08Act
# x=40
# t=20
# i=soa08Act@interest
# m=5
# k=12


# outs<-rLifeContingencies(n,lifecontingency, object, x,t,i=i,
# m=m,k=k, parallel=TRUE)
# APV=IAxn(object, x=x,n=t, k=k)
# mean(outs)
# APV
# t.test(x=outs, mu=APV)


# IAxn(soa08Act, x=50,n=10,k=2,type="EV")
# IAxn(soa08Act, x=50,n=10,k=2,type="ST")

DAxn <- function(actuarialtable, x, n,i = actuarialtable@interest, m = 0, k = 1, type =
             "EV",power = 1)
  {
    out <- numeric(1)
    interest <- i
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable")
    if (missing(x))
      stop("Error! Need age!")
    type <- testtyperesarg(type)
    if (missing(m))
      m = 0
    if (missing(n))
      n = getOmega(actuarialtable) - x - m #n=getOmega(actuarialtable)-x-m-1
    
    
    payments = seq(from = n, to = 1 / k, by = -1 / k)
    probs = numeric(n * k)
    times = m + seq(from = 0, to = (n - 1 / k),by = 1 / k)
    startAge = x #we start from x
    for (i in 1:length(times))
      probs[i] = (
        pxt(object = actuarialtable, x = startAge,t = times[i]) * qxt(
          object = actuarialtable, x = startAge + times[i],t = 1 / k
        )
      )
    discounts = (1 + interest) ^ -(times + 1 / k)
    
    if (type == "EV") {
      out <- sum(((payments * discounts) ^ power) * probs)
    } else if (type == "ST")
    {
      out = rLifeContingencies(
        n = 1,lifecontingency = "DAxn",
        object = actuarialtable, x = x,t = n,i = interest, m = m,k = k
      )
    }
    return(out)
  }

# n=100000
# lifecontingency="DAxn"
# actuarialtable=soa08Act
# object=actuarialtable
# x=40
# t=20
# i=soa08Act@interest
# m=0
# k=1

# outs<-rLifeContingencies(n,lifecontingency, object, x,t,i=i,
# m=m,k=k, parallel=TRUE)
# APV=IAxn(object, x=x,n=t, k=k)
# mean(outs)
# APV
# t.test(x=outs, mu=APV)



#n-year increasing
#recursive function
Iaxn <- function(actuarialtable, x, n,i = actuarialtable@interest, m = 0, type =
             "EV", power = 1)
  {
    out <- numeric(1)
    interest <- i
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable")
    type <- testtyperesarg(type)
    if (missing(m))
      m = 0
    if (missing(x))
      stop("Error! Need age!")
    #m is set equal to zero at the moment
    if (missing(n))
      n = getOmega(actuarialtable) - x - m #n=getOmega(actuarialtable)-x-m-1
    
    #i an interest rate is provided the provided interest rate overrides the
    #actuarialtable interest rate
    payments = numeric(n)
    probs = numeric(n)
    times = numeric(n)
    discounts = numeric(n)
    
    payments = seq(from = 1, to = n, by = 1)
    times = m + seq(from = 0, to = (n - 1),by = 1)
    for (i in 1:length(times))
      probs[i] = pxt(actuarialtable, x,times[i])
    discounts = (1 + interest) ^ -(times)
    out <- sum(((payments * discounts) ^ power) * probs)
    return(out)
  }



#pure endowment function
AExn <- function(actuarialtable, x, n,i = actuarialtable@interest, k = 1, type =
             "EV",power = 1)
  {
    out <- numeric(1)
    interest <- i
    if (missing(actuarialtable))
      stop("Error! Need an actuarial actuarialtable")
    if (missing(x))
      stop("Error! Need age!")
    type <- testtyperesarg(type)
    if (k < 1)
      stop("Error! Periods in a year shall be no less than 1")
    if (missing(n))
      n = getOmega(actuarialtable) - x - 1
    
    if (n == 0)
      return(1)
    if (any(x < 0,n < 0))
      stop("Error! Negative parameters")
    
    if (type == "EV") {
      out <-
        Axn(
          actuarialtable = actuarialtable, x = x, n = n, i = i,m = 0, k = k,type =
            "EV",power = power
        ) + Exn(
          actuarialtable = actuarialtable, x = x, n = n, i = i,
          type = "EV",power = power
        )
    } else if (type == "ST") {
      out = rLifeContingencies(
        n = 1,lifecontingency = "AExn",
        object = actuarialtable, x = x,t = n,i = interest, k = k
      )
    }
    return(out)
  }


#x=35
#t=30
#n=200000
#object=soa08Act
#lifecontingency="EAxn"
#i=0.06
#out<-rLifeContingencies(n=20000,lifecontingency="Axn", object=soa08Act, x=35,
#		t=30,i=0.06,k=1, parallel=TRUE)
