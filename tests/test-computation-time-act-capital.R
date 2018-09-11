library(lifecontingencies)

data("soa08Act")

#test accuraccy
AXn <- Vectorize(lifecontingencies:::Axnold, "x")
AxN <- Vectorize(lifecontingencies:::Axnold, "n")
AxnM <- Vectorize(lifecontingencies:::Axnold, "m")

Axnvect <- Axn

AxncheckR <- function(object, x, m, n)
{
  i <- object@interest
  f <- function(t)
    tpx_q_xt <- pxt(object, x=x, t=t)*qxt(object, x=x+t, t=1)
  prob <- sapply(m:(m+n-1), f)
  rowSums(prob /( (1+i)^(m+1):(m+n)))
}

#basic check
cbind(
AXn(soa08Act, 65:66, n=1, m=1) ,
Axnvect(soa08Act, 65:66, n=1, m=1),
AxncheckR(soa08Act, 65:66, m=1, n=1)
)

#k > 1
x <- 85
cbind(x=x, Axnvect(soa08Act, x=x, k=3), AXn(soa08Act, x=x, k=3))

#non-integer age
x <- 30:35+1/2
cbind(x=x, Axnvect(soa08Act, x=x), AXn(soa08Act, x=x))


#from testActuarialMathematics

x <- 0:9
lx <- seq(100, 10, by = -10)
tbl <- new("actuarialtable", x = x, lx = lx, interest = 0, name = "Uniformly decreasing lx")


c(1, AXn(tbl, x = 0), Axnvect(tbl, x = 0))

c(1, AXn(tbl, x = 8.3, k = 1), Axnvect(tbl, x = 8.3, k = 1))


v <- 1.06^(-seq(0.5, 2.0, by = 0.5))
p <- c(rep(5/17,3), 2/17)
ans <- sum(p * v)

c(AXn(tbl, x = 8.3, k = 2, i = 0.06), Axnvect(tbl, x = 8.3, k = 2, i = 0.06), ans)

v <- 1.06^(-seq(0.5, 1.5, by = 0.5))
p <- c(rep(5/13,2), 3/13)
ans <- sum(p * v)
c(AXn(tbl, x = 8.7, k = 2, i = 0.06), Axnvect(tbl, x = 8.7, k = 2, i = 0.06), ans)


round(c(Axn(soa08Act,x=30,i=0.06,k=4),
        AXn(soa08Act,x=30,i=0.06,k=4),
        0.1048),4) # 0.1048) #BOWERS P 339


#check previous and old

checkvalx <- function()
  all(Axnvect(soa08Act, x=1:100) == AXn(soa08Act, x=1:100))
checkvaln <- function()
  all(Axnvect(soa08Act, x=33, n=10:50) == AxN(soa08Act, x=33, n=10:50))
checkvalm <- function()
  all(Axnvect(soa08Act, x=33, m=0:50) == AxnM(soa08Act, x=33, m=0:50))

checkvalk <- function(k)
{
  allx <- seq(1, 50, by=0.5)
  new <- Axnvect(soa08Act, x=allx, k=k)
  old <- AXn(soa08Act, x=allx, k=k)
  #print(cbind(old=old, new=new))
  cbind("equal on all digit"=all(old == new), "equal with round off"=  sum(abs(old - new)) < 1e-6)
}

checkvalx()
checkvaln()
checkvalm()
checkvalk(2)
checkvalk(4)
#checkvalk(6)
#checkvalk(12)

nrep <- 5
system.time(replicate(nrep, Axnvect(soa08Act, x=1:100) ))
system.time(replicate(nrep, AXn(soa08Act, x=1:100) ))

system.time(replicate(nrep, Axnvect(soa08Act, x=33, n=1:50) ))
system.time(replicate(nrep, AxN(soa08Act, x=33, n=1:50) ))

