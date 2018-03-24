#__________________________________________________
#test file

require(lifecontingencies)

#__________________________________________________
#vectorize function for testing purposes
pXt <- Vectorize(pxt, "x")
qXt <- Vectorize(qxt, "x")
pxT <- Vectorize(pxt, "t")
qxT <- Vectorize(qxt, "t")

tol <- (.Machine$double.eps)^(1/3)

#see file R/testfunc-demographic.R

getpxqx <- lifecontingencies:::getpxqx

gettpx <- lifecontingencies:::gettpx

gettqx <- lifecontingencies:::gettqx

getkpxqxk <- lifecontingencies:::getkpxqxk




#US life table 1979-1981 p 60 Actuarial mathematics
x <- 0:109
lx <- c(100000, 
  98000+ c(740, 648, 584, 535, 495, 459, 426, 396, 370, 347, 328, 309, 285, 248, 196, 129, 047),
  97000+ c(953, 851, 741, 623, 499, 370, 240, 110),
  96000+ c(982, 856, 730, 604, 477, 350, 220, 88),
  95000+ c(951, 808, 655, 492, 317, 129), 
  94000+ c(926, 706, 465, 201),
  93000+ c(913, 599, 256),
  92000+ c(882, 472, 21), 91526, 90986, 90402, 89771, 89087, 88348, 87551, 86695, 85776, 84789,
  83726, 82726, 81348, 80024, 78609, 77107, 75520, 73846, 72082, 70218, 68248, 66165, 63972, 61673,
  59279, 56799, 54239, 51599, 48878, 46071, 43180, 40208, 37172, 34095, 31012, 27960, 24961, 22038,
  19235, 16598, 14154, 11908, 9863, 8032, 6424, 5043, 3884, 2939, 2185, 1598, 1150, 815, 570, 393,
  267, 179, 119, 78, 51, 33)

usLT7981 <- data.frame(x=x, lx=lx)
USLT7981 <- new("lifetable", x=x, lx=lx)


#__________________________________________________
#living prob (one-year)
stopifnot(
  sum(abs(
    pXt(USLT7981, x=head(x, -1), t=1) - getpxqx(usLT7981$lx)[, "px"])
    ) < tol
)

#living prob (t-year) at age x
stopifnot(
  all(
    sapply(c(5, 10, 15, 20, 25, 30, 60, 70), function(k)
	sum(abs(pxT(USLT7981, x=k, t=0:(max(x)-k)) - getkpxqxk(usLT7981$lx, x=k)[, "kpx"])) 
	) < tol)
)


#fractional year living prob

myt <- 1:30/8

stopifnot(
  sum(abs(
    cbind(
      pxT(USLT7981, x=10, t=myt, fractional="linear") - gettpx(usLT7981$lx, x=10, k=myt, fractional="linear"),
      pxT(USLT7981, x=10, t=myt, fractional="hyperbolic") - gettpx(usLT7981$lx, x=10, k=myt, fractional="balducci"),
      pxT(USLT7981, x=10, t=myt, fractional="constant force") - gettpx(usLT7981$lx, x=10, k=myt, fractional="constant")
    )
  )) < tol
)

#fractional year death prob

stopifnot(
  sum(abs(
    cbind(
      qxT(USLT7981, x=10, t=myt, fractional="linear") - gettqx(usLT7981$lx, x=10, k=myt, fractional="linear"),
      qxT(USLT7981, x=10, t=myt, fractional="hyperbolic") - gettqx(usLT7981$lx, x=10, k=myt, fractional="balducci"),
      qxT(USLT7981, x=10, t=myt, fractional="constant force") - gettqx(usLT7981$lx, x=10, k=myt, fractional="constant")
    )
  )) < tol
)



#__________________________________________________
#death prob (one-year)
stopifnot(
  sum(abs(qXt(USLT7981, x=head(x, -1), t=1) - getpxqx(usLT7981$lx)[, "qx"])) < tol
)

#death prob (one-year) at age x+k
stopifnot(
  sum(
    sapply(c(5, 10, 15, 20, 25, 30), function(k)
      sum(abs(qXt(USLT7981, x=k+0:(max(x)-k), t=1) - getkpxqxk(usLT7981$lx, x=k)[, "qxplusk"]))
    )) < tol
)

#__________________________________________________
#residual life expectancy
ex <- function(x)
  sum(getkpxqxk(usLT7981$lx, x=x)[-1, "kpx"])

stopifnot(
  sum(
    sapply(0:20, function(x) abs(ex(0) - exn(USLT7981, 0)))
  ) < tol
)

