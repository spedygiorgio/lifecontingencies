
#__________________________________________________
#local R functions for testing purposes

getrentest <- function(Lx, x, R, nu, s, t, anticipated=TRUE)
{
  if(missing(s))
    s <- 0
  if(missing(t))
    t <- Inf
  kpx <- getkpxqxk(Lx, x)
  if(anticipated)
    idx <- kpx[,"k"] >= s & kpx[,"k"] < s+t
  else
    idx <- kpx[,"k"] > s & kpx[,"k"] <= s+t
  
  
  R * sum(nu^kpx[idx,"k"] * kpx[idx,"kpx"])
  
}


getcapitalst <- function(Lx, x, K, nu, s, t)
{
  if(missing(s))
    s <- 0
  if(missing(t))
    t <- Inf
  kpx <- getkpxqxk(Lx, x)
  idx <- kpx[,"k"] >= s & kpx[,"k"] < s+t	
  K*sum(nu^(kpx[idx,"k"]+1) * kpx[idx,"kpx"] * kpx[idx,"qxplusk"])
}


getrentestthly <- function(Lx, x, R, nu, s, t, k, fractional=c("linear", "balducci", "constant"), anticipated=TRUE)
{
  if(missing(s))
    s <- 0
  if(missing(t))
    t <- Inf
  if(anticipated)	
    l <- seq(s*k, (s+t)*k-1, by=1)
  else
    l <- seq(s*k+1, (s+t)*k, by=1)
  nuk <- nu^(1/k)
  years <- seq(s, s+t-1, by=1)
  fracyear <- (0:(k-1))/k
  
  lpx <- as.vector(sapply(years, function(y) gettpx(Lx, x, y+fracyear, fractional)))
  
  R/k*sum(nuk^(l) * lpx )
}

getcapitalstthly <- function(Lx, x, K, nu, s, t, k, fractional=c("linear", "balducci", "constant"))
{
  if(missing(s))
    s <- 0
  if(missing(t))
    t <- Inf
  l <- seq(s*k, (s+t)*k-1, by=1)
  nuk <- nu^(1/k)
  years <- seq(s, s+t-1, by=1)
  fracyear <- (0:(k-1))/k
  
  lpx <- as.vector(sapply(years, function(y) gettpx(Lx, x, y+fracyear, fractional)))
  qxl <- as.vector(sapply(years, function(y) gettqx(Lx, x+y+fracyear, 1/k, fractional)))
  
  K*sum(nuk^(l+1) * lpx * qxl)
}

