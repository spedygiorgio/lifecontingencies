#__________________________________________________
#test file

require(lifecontingencies)

#__________________________________________________


tol <- (.Machine$double.eps)^(1/3)


#see file R/testfunc-financial.R

getrentest <- lifecontingencies:::getrentest

getcapitalst <- lifecontingencies:::getcapitalst

getrentestthly <- lifecontingencies:::getrentestthly

getcapitalstthly <- lifecontingencies:::getcapitalstthly


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
#death capital life insurance

i <- 3/100
x <- 10
m <- 0
n <- 10

#defered of m period, temporary of n period at age x
stopifnot(
  sum(
    sapply(c(0, 5, 10, 15, 20, 25, 30), function(x)
      sapply(1:10, function(m)
        sum(abs(
          sapply(1:10, function(n)
            getcapitalst(usLT7981$lx, x=x, K=1, nu=1/(1+i), s=m, t=n) - Axn(USLT7981, x=x, n=n, m=m, k=1, i=i)
          ))) 
      ))
  )< tol
)

#__________________________________________________
#death capital life insurance (monthly)


i <- 3/100
x <- 0
m <- 0
n <- 1

sum(
sapply(c(0, 5, 10, 15, 20, 25, 30), function(x)
  abs(
getcapitalstthly(usLT7981$lx, x=x, K=1, nu=1/(1+i), s=m, t=n, k=12, frac="linear") - Axn(USLT7981, x=x, n=n, m=m, k=12, i=i)
)
)
)< tol

#__________________________________________________
#annuity life insurance (monthly)


i <- 3/100
x <- 20
m <- 0
n <- 2

#anticipated 
sum(
sapply(1:10, function(m)
sapply(1:10, function(n)
abs(getrentestthly(usLT7981$lx, x=x, R=1, nu=1/(1+i), s=m, t=n, k=12, frac="linear", anticipated=TRUE) - axn(USLT7981, x=x, n=n, m=m, k=12, i=i, pay="due") )
)
)
)< tol


#delayed 
sum(abs(
sapply(1:10, function(m)
sapply(1:10, function(n)
getrentestthly(usLT7981$lx, x=x, R=1, nu=1/(1+i), s=m, t=n, k=12, frac="linear", anticipated=FALSE) - axn(USLT7981, x=x, n=n, m=m, k=12, i=i, pay="immediate") 
)
)
)) < tol


