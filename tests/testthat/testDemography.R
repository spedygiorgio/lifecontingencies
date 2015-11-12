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
