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
###         testing functions
###

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

