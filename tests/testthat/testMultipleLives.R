library(lifecontingencies)

context("Multiple lives")
data(soa08Act)

test_that("Life Insurance", {
  expect_equal(round(Axyzn(tablesList = list(soa08Act,soa08Act),x=c(60,70),status="last"),5), 0.3118) #FINAN 617
  expect_equal(round(axyzn(tablesList = list(soa08Act,soa08Act),x=c(60,70),status="joint"),4), 7.5563) #FINAN 617
  expect_equal(round(axyzn(tablesList = list(soa08Act,soa08Act),x=c(50,60),status="last"),4), 14.2178) #FINAN 617
  expect_equal(round(axyzn(tablesList = list(soa08Act,soa08Act),x=c(30,40),status="joint",m=1,n = 10),2),7.1687) #FINAN 619
  expect_equal(round(axn(soa08Act,x=50)+axn(soa08Act,x=60)-axyzn(tablesList = list(soa08Act,soa08Act),x=c(50,60),status="joint")-axyzn(tablesList = list(soa08Act,soa08Act),x=c(50,60),status="last"),5),0) #GENERAL
})


