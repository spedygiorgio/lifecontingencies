library(lifecontingencies)

data("soa08Act")

listsoa <- list(soa08Act, soa08Act, soa08Act)

#test accuraccy
pxyztvect <- pxyzt
pxyzt_matx <- function(x, ...) 
  sapply(1:NROW(x), function(i) lifecontingencies:::pxyztold(x=x[i,], ...))
pxyzt_matxt <- function(x, t, ...) 
  sapply(1:NROW(x), function(i) lifecontingencies:::pxyztold(x=x[i,], t=t[i,1], ...))

checkvalx <- function(fractional)
{
  x <- cbind(1:120, 11:20, 91:120)
  new <- pxyztvect(listsoa, x=x, t=1/4, fractional = fractional)
  old <- pxyzt_matx(x, tablesList=listsoa, t=1/4, fractional = fractional)
  #print(cbind(new, old))
  sum(abs(new-old)) < 1e-6
}


checkvalx("linear")
checkvalx("harm")
checkvalx("exp")

checkvalxt <- function(fractional)
{
  x <- cbind(1:120, 11:20, 91:120)
  t <- cbind(1:20, 1:20, 1:20)
  t <- rbind(t,t,t,t,t,t)+1/2
  new <- pxyztvect(listsoa, x=x, t=t, fractional = fractional)
  old <- pxyzt_matxt(x, tablesList=listsoa, t=t, fractional = fractional)
  #print(cbind(new, old))
  sum(abs(new-old)) < 1e-6
}
checkvalxt("linear")
checkvalxt("harm")
checkvalxt("exp")


x <- cbind(1:120, 11:20, 91:120)
t <- cbind(1:20, 1:20, 1:20)
t <- rbind(t,t,t,t,t,t)+1/2

nrep <- 10
system.time(replicate(nrep, pxyztvect(listsoa, x=x, t=t) ))
system.time(replicate(nrep, pxyzt_matxt(x, tablesList=listsoa, t=t) ))


listsoa <- list(soa08Act, soa08Act, soa08Act, soa08Act, soa08Act, soa08Act)
x <- cbind(x, x)
t <- cbind(t, t)

system.time(replicate(nrep, pxyztvect(listsoa, x=x, t=t) ))
system.time(replicate(nrep, pxyzt_matxt(x, tablesList=listsoa, t=t) ))

