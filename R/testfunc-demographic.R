
#__________________________________________________
#local functions for testing purposes (not exported at all)
getpxqx <- function(Lx)
{
  px <- Lx[-1] / Lx[-length(Lx)]
  cbind(x=0:(length(Lx)-2), px=px, qx=1-px)
}

frac <- function(x) x-floor(x)

gettpx <- function(Lx, x, k, fractional=c("linear", "balducci", "constant"))
{
  fractional <- match.arg(fractional, c("linear", "balducci", "constant"))
  
  #Lx : 0-> omega-1 so Lx[floor(x)+1] is L_x
  Flinear <- function(k)
  {
    pxleft <- Lx[floor(x)+floor(k)+1] / Lx[floor(x)+1]
    pxright <- Lx[floor(x)+ceiling(k)+1] / Lx[floor(x)+1]
    pxleft *(1-frac(k)) + frac(k)*pxright
  }
  Fbalducci <- function(k)
  {
    kpx <- Lx[floor(x)+floor(k)+1] / Lx[floor(x)+1]
    pxk <- Lx[floor(x)+ceiling(k)+1] / Lx[floor(x)+floor(k)+1]
    qxk <- 1-pxk
    kpx* pxk/(1-(1-frac(k))*qxk)
  }
  Fconstant <- function(k)
  {
    kpx <- Lx[floor(x)+floor(k)+1] / Lx[floor(x)+1]
    pxk <- Lx[floor(x)+ceiling(k)+1] / Lx[floor(x)+floor(k)+1]
    kpx * pxk^frac(k)
  }
  
  if(fractional == "linear")
    res <- sapply(k, Flinear)
  else if(fractional == "balducci")
    res <- sapply(k, Fbalducci)
  else #constant
    res <- sapply(k, Fconstant)
  
  res[k < 0] <- NaN
  res
}

gettqx <- function(Lx, x, k, fractional=c("linear", "balducci", "constant"))
{
  1-gettpx(Lx, x, k, fractional=c("linear", "balducci", "constant"))
}

getkpxqxk <- function(Lx, x)
{
  matx <- getpxqx(Lx)
  
  pxplus <- matx[,"px"][(x+1):length(matx[,"px"])]
  cbind(k=0:length(pxplus), kpx=c(1, cumprod(pxplus)), 
        qxplusk=c(1-pxplus, 1))
}