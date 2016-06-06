library(lifecontingencies)

context("Demography")
data(soa08Act)

soa08qx<-as(soa08Act,"numeric")
soa08mx<-qx2mx(qx=soa08qx)
soa08qx2<-mx2qx(soa08mx)

test_that("qx and mx", {
 expect_equal(soa08qx,soa08qx2)
  expect_equal(round(pxt(soa08Act, 65,15)-pxt(soa08Act, 65,25),4),0.3791)
  expect_equal(round(exn(soa08Act,x=70,n = 2),5),1.89858)
})

test_that("Survival probabilities are correct in simple uniform mortality table",{
  l <- function(x) return(max(0, 100 - 10 * x))
  f <- function(x, t) return(l(x+t)/l(x))
  
  x <- 0:9
  lx <- seq(100, 10, by = -10)
  tbl <- new("lifetable",x = x, lx = lx, name = "Uniform mortality")
  
  expect_equal(pxt(tbl, x = 0, t = 2.4), f(0,2.4))
  expect_equal(pxt(tbl, x = 7, t = 0), f(7,0))
  expect_equal(pxt(tbl, x = 7, t = 1.9), f(7,1.9))
})

test_that("pxt and pxyzt return equal results for a single table", {
  x <- 0:9
  lx <- seq(100, 10, by = -10)
  tbl <- new("lifetable",x = x, lx = lx, name = "Uniform mortality")
  
  expect_equal(pxt(tbl, x = 5, t = 3.1), pxyzt(list(tbl), x = c(5), t = c(3.1)))
  expect_equal(pxt(tbl, x = 1, t = 3.1), pxyzt(list(tbl), x = c(1), t = c(3.1)))
  expect_equal(pxt(tbl, x = 2.4, t = 5.3), pxyzt(list(tbl), x = c(2.4), t = c(5.3)))
})