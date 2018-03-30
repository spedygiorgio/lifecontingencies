library(lifecontingencies)

context("Financial Mathematics")

test_that("Annuities", {
  expect_equal(round(decreasingAnnuity(i = 0.03,n = 10,type = "due")*10,2), 504.63) #BOWERS P 339
})

#TODO: ADD DURATION CHECKS

