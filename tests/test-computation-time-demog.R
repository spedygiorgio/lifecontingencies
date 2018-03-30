library(lifecontingencies)

data("soa08Act")

#test access functions
f <- function(x)
  soa08Act@lx[which(soa08Act@x == x)] 

g <- function(j)
  if(soa08Act@x[1] == 0)
    soa08Act@lx[soa08Act@x[j+2]]
if(all(g(1:10) == sapply(1:10, f)))
{
  system.time(replicate(1e3, sapply(0:139, f)))
  system.time(replicate(1e3, g(0:139) ))
}

#test accuraccy
pXt <- Vectorize(pxt, "x")
pxT <- Vectorize(pxt, "t")
pxtvect <- lifecontingencies:::pxtvect

#high-age
cbind(x=135:145, lx=g(135:145), pxtvect(soa08Act, x=135:145, t=1), pXt(object=soa08Act, x=135:145, t=1))

#non-integer age
cbind(x=10+0:6/6, lx=g(10+0:6/6), pxtvect(soa08Act, x=10+0:6/6, t=1), pXt(object=soa08Act, x=10+0:6/6, t=1))


checkvalx <- function(fractional)
  all(pxtvect(soa08Act, x=1:100, t=1/3, fractional = fractional) == pXt(object=soa08Act, x=1:100, t=1/3, fractional = fractional))
checkvalt <- function(fractional)
  all(pxtvect(soa08Act, x=2, t=1:100/3, fractional = fractional) == pxT(object=soa08Act, x=2, t=1:100/3, fractional = fractional))

c(checkvalx("linear"), checkvalt("linear"))
c(checkvalx("harm"), checkvalt("harm"))
c(checkvalx("exp"), checkvalt("exp"))

system.time(replicate(1e3, pxtvect(soa08Act, x=1:130, t=1/2) ))
system.time(replicate(1e3, pXt(soa08Act, x=1:130, t=1/2) ))

system.time(replicate(1e3, pxtvect(soa08Act, x=1, t=1:130/2) ))
system.time(replicate(1e3, pxT(soa08Act, x=1, t=1:130/2) ))

