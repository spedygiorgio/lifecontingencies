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