library(lifecontingencies)

data("soa08Act")

#test accuraccy
aXn <- Vectorize(lifecontingencies:::axnold, "x")
axN <- Vectorize(lifecontingencies:::axnold, "n")
axnM <- Vectorize(lifecontingencies:::axnold, "m")

axnvect <- axn

#high-age
x <- 85:90
cbind(x=x, axnvect(soa08Act, x=x, pay="advance"), aXn(soa08Act, x=x, pay="advance"))

#non-integer age
x <- 30:35+1/2
cbind(x=x, axnvect(soa08Act, x=x, pay="advance"), aXn(soa08Act, x=x, pay="advance"))


#strange arguments
x <- 85:90
cbind(x=x, axnvect(soa08Act, x=x, n=c(0,10), m=c(0,1, 2), pay="advance"))


checkvalx <- function(payment)
  all(axnvect(soa08Act, x=1:100, payment=payment) == aXn(soa08Act, x=1:100, payment=payment))
checkvaln <- function(payment)
  all(axnvect(soa08Act, x=33, n=10:50, payment=payment) == axN(soa08Act, x=33, n=10:50, payment=payment))
checkvalm <- function(payment)
  all(axnvect(soa08Act, x=33, m=0:50, payment=payment) == axnM(soa08Act, x=33, m=0:50, payment=payment))

c(checkvalx("due"), checkvalx("arrears"), checkvalx("immediate"), checkvalx("advance"))
c(checkvaln("due"), checkvaln("arrears"), checkvaln("immediate"), checkvaln("advance"))
c(checkvalm("due"), checkvalm("arrears"), checkvalm("immediate"), checkvalm("advance"))

nrep <- 10
system.time(replicate(nrep, axnvect(soa08Act, x=1:100, pay="advance") ))
system.time(replicate(nrep, aXn(soa08Act, x=1:100, pay="advance") ))

system.time(replicate(nrep, axnvect(soa08Act, x=33, n=1:50, pay="advance") ))
system.time(replicate(nrep, axN(soa08Act, x=33, n=1:50, pay="advance") ))

