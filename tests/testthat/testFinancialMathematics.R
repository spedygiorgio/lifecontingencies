library(lifecontingencies)

context("Financial Mathematics")

test_that("Annuities", {
  expect_equal(round(decreasingAnnuity(i = 0.03,n = 10,type = "due")*10,2), 504.63) #BOWERS P 339
})

#TODO: ADD DURATION CHECKS

ex_time = seq(1,6)
ex_cfs = c(rep(50,5),1050)
#http://www.investinganswers.com/financial-dictionary/bonds/duration-1288
test_that("Duration",
          {expect_equal(round(duration(cashFlows = ex_cfs,timeIds = ex_time,i=0.05,macaulay = TRUE),2),5.33)}
          )