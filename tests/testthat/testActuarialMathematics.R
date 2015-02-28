library(lifecontingencies)

context("Actuarial Mathematics")
data(soa08Act)

test_that("Life Insurance", {
  expect_equal(round(Axn(soa08Act,x=30,n=10,i=0.04),6), 0.015773) #BOWERS P 111
  expect_equal(round(Axn(soa08Act,x=30,n=10,i=0.04,power=2)-Axn(soa08Act,x=30,n=10,i=0.04,power=1)^2,6), 0.012471) #BOWERS P 112
  expect_equal(round(1000*Axn(soa08Act,x=30,i=0.06),4), 102.4835) #BOWERS P 112
  expect_equal(round(Axn(soa08Act,x=30,i=0.06,k=4),4), 0.1048) #BOWERS P 339
  expect_equal(round(IAxn(soa08Act,x=50,i=.06),5), 4.99676) #
})

test_that("Annuities", {
  expect_equal(round(Axn(soa08Act,36)/axn(soa08Act,36),5), 0.00881) #FINAN P 437
  expect_equal(round(axn(soa08Act,x=60,k=12),5), 10.68036) #FINAN P 437
  expect_equal(round(axn(soa08Act, x=65,i=0.06),5),9.89693)
})
