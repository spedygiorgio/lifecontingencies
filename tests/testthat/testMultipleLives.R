library(lifecontingencies)

context("Multiple lives")
data(soa08Act)

test_that("Life Insurance", {
  expect_equal(round(Axyzn(tablesList = list(soa08Act,soa08Act),x=c(60,70),status="last"),5), 0.3118) #FINAN 617
  expect_equal(round(axyzn(tablesList = list(soa08Act,soa08Act),x=c(60,70),status="joint"),4), 7.5563) #FINAN 617
  expect_equal(round(axyzn(tablesList = list(soa08Act,soa08Act),x=c(50,60),status="last"),4), 14.2178) #FINAN 617
  expect_equal(round(axyzn(tablesList = list(soa08Act,soa08Act),x=c(30,40),status="joint",m=1,n = 10),3),7.169) #FINAN 619
  expect_equal(round(axn(soa08Act,x=50)+axn(soa08Act,x=60)-axyzn(tablesList = list(soa08Act,soa08Act),x=c(50,60),status="joint")-axyzn(tablesList = list(soa08Act,soa08Act),x=c(50,60),status="last"),5),0) #GENERAL
})

test_that("SOA Illustrative Life Table", {
  data("soa08Act")
  tbls <- list(soa08Act, soa08Act)
  f <- function(x) return(c(axyzn(tbls, x = c(x, x)),
                            Axyzn(tbls, x = c(x, x)) * 1000,
                            Axyzn(tbls, x = c(x, x), power = 2) * 1000,
                            axyzn(tbls, x = c(x, x+10)),
                            Axyzn(tbls, x = c(x, x+10)) * 1000,
                            Axyzn(tbls, x = c(x, x+10), power = 2) * 1000))

  expect_equal(f( 40), c(13.60357, 229.9867,  84.8858, 12.47840, 293.6755, 123.8024), tolerance = 0.000001)
  expect_equal(f( 47), c(12.29706, 303.9398, 130.6661, 10.93105, 381.2615, 187.4810), tolerance = 0.000001)
  expect_equal(f( 53), c(10.95797, 379.7377, 186.2752,  9.42400, 466.5661, 260.4567), tolerance = 0.000001)
  expect_equal(f( 61), c( 8.92659, 494.7213, 286.9070,  7.28853, 587.4417, 382.4614), tolerance = 0.000001)
  expect_equal(f( 78), c( 4.57002, 741.3197, 571.4091,  3.34060, 810.9096, 670.7874), tolerance = 0.000001)
  expect_equal(f( 84), c( 3.36075, 809.7690, 669.0804,  2.41251, 863.4431, 752.4921), tolerance = 0.000001)
  expect_equal(f( 95), c( 1.85998, 894.7179, 804.2185,  1.39571, 920.9973, 849.6744), tolerance = 0.000001)
  expect_equal(f(102), c( 1.34659, 923.7777, 854.5980,  1.11241, 937.0336, 878.3888), tolerance = 0.000001)
  expect_equal(f(110), c( 1.07154, 939.3470, 882.5952,  1.00934, 942.8678, 889.0280), tolerance = 0.000001)
})