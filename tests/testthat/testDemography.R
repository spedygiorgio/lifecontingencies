library(lifecontingencies)

context("Demography")
data(soa08Act)

soa08qx<-as(soa08Act,"numeric")
soa08mx<-qx2mx(qx=soa08qx)
soa08qx2<-mx2qx(soa08mx)

test_that("qx and mx", {
 expect_equal(soa08qx,soa08qx2)
})