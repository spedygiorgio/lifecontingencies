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