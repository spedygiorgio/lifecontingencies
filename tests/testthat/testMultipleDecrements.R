library(lifecontingencies)

context("multiple decrements")
#Initializing: creating the valdez example
valdezDf<-data.frame(
  x=c(50:54),
  lx=c(4832555,4821937,4810206,4797185,4782737),
  heart=c(5168, 5363, 5618, 5929, 6277),
  accidents=c(1157, 1206, 1443, 1679,2152),
  other=c(4293,5162,5960,6840,7631)
)
valdezMdt<-new("mdt",name="ValdezExample",table=valdezDf)

test_that("basic demographics on mdt objects", {
  expect_equal(dxt(valdezMdt,x=51,t=2,decrement = "other"),11122)
  expect_equal(round( pxt(valdezMdt,x=50,t=3),5),0.99268)
  expect_equal(round(qxt(valdezMdt,x=50,t=3, decrement = "heart"),5),0.00334)
})

test_that("qxprimevarious", {
  expect_equal(qxt.fromQxprime(qx.prime = 0.01,other.qx.prime = c(0.03,0.06)),0.009556)
})

test_that("t=0 yields no decrement and full survival, with or without a decrement", {
  expect_equal(dxt(valdezMdt, x = 51, t = 0), 0)
  expect_equal(dxt(valdezMdt, x = 51, t = 0, decrement = "other"), 0)
  expect_equal(qxt(valdezMdt, x = 51, t = 0), 0)
  expect_equal(qxt(valdezMdt, x = 51, t = 0, decrement = "other"), 0)
  expect_equal(pxt(valdezMdt, x = 51, t = 0), 1)
  expect_equal(pxt(valdezMdt, x = 51, t = 0, decrement = "other"), 1)
})

test_that("an unrecognized decrement name errors consistently across dxt/qxt/pxt", {
  expect_error(dxt(valdezMdt, x = 51, t = 1, decrement = "doesnotexist"))
  expect_error(qxt(valdezMdt, x = 51, t = 1, decrement = "doesnotexist"))
  expect_error(pxt(valdezMdt, x = 51, t = 1, decrement = "doesnotexist"))
})

test_that("bottomCompletionSurvival only affects the synthetic backfill below the supplied ages", {
  mdtDefault <- new("mdt", name = "default", table = valdezDf)
  mdtCustom  <- new("mdt", name = "custom", table = valdezDf, bottomCompletionSurvival = 0.95)

  # the two tables must diverge on the synthetic ages...
  expect_false(isTRUE(all.equal(
    mdtDefault@table$lx[mdtDefault@table$x == 0],
    mdtCustom@table$lx[mdtCustom@table$x == 0]
  )))

  # ...but agree exactly on the originally supplied data (age 50 onward)
  expect_equal(
    mdtDefault@table$lx[mdtDefault@table$x == 50],
    mdtCustom@table$lx[mdtCustom@table$x == 50]
  )
  expect_equal(mdtDefault@table$lx[mdtDefault@table$x == 50], 4832555)

  # actuarial quantities computed on the originally supplied age range are
  # therefore unaffected by the completion assumption
  expect_equal(
    dxt(mdtDefault, x = 51, t = 2, decrement = "other"),
    dxt(mdtCustom, x = 51, t = 2, decrement = "other")
  )
})

test_that("bottomCompletionSurvival must be a valid probability", {
  expect_error(new("mdt", table = valdezDf, bottomCompletionSurvival = 1.5))
  expect_error(new("mdt", table = valdezDf, bottomCompletionSurvival = -0.1))
})

test_that("Axn.mdt reproduces the Finan (2014, p. 674) example", {
  myTable <- data.frame(
    x = c(16, 17, 18),
    lx = c(20000, 17600, 14520),
    da = c(1300, 1870, 2380),
    doc = c(1100, 1210, 1331)
  )
  myMdt <- new("mdt", table = myTable, name = "Sample")
  expect_equal(round(Axn.mdt(object = myMdt, x = 16, i = .1, decrement = "da"), 4), 0.1364)
})