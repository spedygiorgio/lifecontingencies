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
pXt <- Vectorize(lifecontingencies:::pxtold, "x")
pxT <- Vectorize(lifecontingencies:::pxtold, "t")
pxtvect <- pxt

#non-integer age
compalldigit <- function(x, t, fractional)
  print(
    cbind(x=x,
          new=pxtvect(soa08Act, x=x, t=t, fractional=fractional),
          old=pXt(object=soa08Act, x=x, t=t, fractional=fractional))
    , digits=22)

compalldigit(80+0:6/4, t=1, "exp")
compalldigit(80+0:6/4, t=1, "lin")
compalldigit(80+0:6/4, t=1, "hyp")



cbind(x=10+0:6/6, lx=g(10+0:6/6), pxtvect(soa08Act, x=10+0:6/6, t=1), pXt(object=soa08Act, x=10+0:6/6, t=1))

#high-age
cbind(x=135:145, lx=g(135:145), pxtvect(soa08Act, x=135:145, t=1), pXt(object=soa08Act, x=135:145, t=1))


#non consecutive age
x <- rpois(10, 45)
cbind(x=x, pxtvect(soa08Act, x=x, t=1), pXt(object=soa08Act, x=x, t=1))


checkvalx <- function(fractional)
{
  allfracage <- seq(1, 100, by=1/4)
  new <- pxtvect(soa08Act, x=allfracage, t=1/3, fractional = fractional)
  old <- pXt(object=soa08Act, x=allfracage, t=1/3, fractional = fractional)
  cbind("equal on all digit"=all(old == new), "equal with round off"=  sum(abs(old - new)) < 1e-6)
}  
checkvalt <- function(fractional)
{
  allfractime <- seq(1, 30, by=1/4)
  new <- pxtvect(soa08Act, x=2, t=allfractime, fractional = fractional)
  old <- pxT(object=soa08Act, x=2, t=allfractime, fractional = fractional)
  cbind("equal on all digit"=all(old == new), "equal with round off"=  sum(abs(old - new)) < 1e-6)
}

rbind("lin"=checkvalx("linear"), "harm"=checkvalx("harm"), "exp"=checkvalx("exp"))

rbind("lin"=checkvalt("linear"), "harm"=checkvalt("harm"), "exp"=checkvalt("exp"))


nrep <- 10
system.time(replicate(nrep, pxtvect(soa08Act, x=1:130, t=1/2) ))
system.time(replicate(nrep, pXt(soa08Act, x=1:130, t=1/2) ))

system.time(replicate(nrep, pxtvect(soa08Act, x=1, t=1:130/2) ))
system.time(replicate(nrep, pxT(soa08Act, x=1, t=1:130/2) ))

