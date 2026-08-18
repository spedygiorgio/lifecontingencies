library(lifecontingencies)

context("Financial Mathematics")

test_that("Annuities", {
  expect_equal(round(decreasingAnnuity(i = 0.03,n = 10,type = "due")*10,2), 504.63) #BOWERS P 339
})

test_that("Annuity immediate and due use standard payment timing", {
  i <- 0.03
  n <- 10

  immediate <- annuity(i = i, n = n, type = "immediate")
  due <- annuity(i = i, n = n, type = "due")

  # annuity-immediate pays at t = 1,...,n; annuity-due at t = 0,...,n-1
  expect_equal(immediate, sum((1 / (1 + i))^(1:n)), tolerance = 1e-12)
  expect_equal(due, sum((1 / (1 + i))^(0:(n - 1))), tolerance = 1e-12)
  expect_equal(due, (1 + i) * immediate, tolerance = 1e-12)

  # Backward-compatible aliases must retain the same semantics.
  expect_equal(annuity(i = i, n = n, type = "arrears"), immediate, tolerance = 1e-12)
  expect_equal(annuity(i = i, n = n, type = "advance"), due, tolerance = 1e-12)
})

#TODO: ADD DURATION CHECKS

ex_time = seq(1,6)
ex_cfs = c(rep(50,5),1050)
#http://www.investinganswers.com/financial-dictionary/bonds/duration-1288
test_that("Duration",
          {expect_equal(round(duration(cashFlows = ex_cfs,timeIds = ex_time,i=0.05,macaulay = TRUE),2),5.33)}
          )
