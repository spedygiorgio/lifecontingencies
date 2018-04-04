library(lifecontingencies)
pxtvect <- lifecontingencies:::pxtvect

#strange example of non consecutive integers
x <- 0:10*10
y <- 0:9
lx <- c(100, 99, 98, 97, 96, 93, 86, 73, 50, 17)

try(
freTH0002 <- new("lifetable", x=x, lx=lx)
)

freTH0002 <- new("lifetable", x=y, lx=lx)

pXt <- Vectorize(pxt, "x")

n <- length(y)
cbind(c(lx, 0), c(lx[-1] / lx[-n], 0, 0), pxtvect(freTH0002, x=0:10, t=1), pXt(freTH0002, x=0:10, t=1))


#strange example of plateau
y <- 0:10
lx <- c(100, 99, 98, 97, 96, 96, 96, 96, 96, 96, 96)

freTH0002 <- new("lifetable", x=y, lx=lx)

n <- length(x)
cbind(c(lx[-1] / lx[-n], 0), pxtvect(freTH0002, x=y, t=1), pXt(freTH0002, x=y, t=1))

