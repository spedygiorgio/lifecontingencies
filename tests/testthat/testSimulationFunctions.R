library(lifecontingencies)
context("Simulation Functions")

T<-seq(from=0,to=100,by=10)
y<-seq(from=0,to=100,by=10)
k<-seq(from=1,to=12,by=3)
i<-0.03
m<-seq(from=0,to=20,by=5)
t<-seq(from=0,to=40, by=5)
n<-seq(from=0,to=20, by=5)
myParGrid<-expand.grid(T=T,y=y,k=k,i=i,m=m,t=t,n=n)

outR<-with(myParGrid,mapply(FUN = lifecontingencies:::.fAxn,T=T,y=y,n=n,i=i,m=m,k=k))
outCpp<-with(myParGrid,mapply(FUN = lifecontingencies:::.fAxnCpp,T=T,y=y,n=n,i=i,m=m,k=k))

#testing Axn

test_that("Axn", {
  expect_equal(outR,outCpp) 
})


#testing IAxn

outR<-with(myParGrid,mapply(FUN = lifecontingencies:::.fIAxn,T=T,y=y,n=n,i=i,m=m,k=k))
outCpp<-with(myParGrid,mapply(FUN = lifecontingencies:::.fIAxnCpp,T=T,y=y,n=n,i=i,m=m,k=k))

test_that("IAxn", {
  expect_equal(outR,outCpp) 
})

#testing DAxn

outR<-with(myParGrid,mapply(FUN = lifecontingencies:::.fDAxn,T=T,y=y,n=n,i=i,m=m,k=k))
outCpp<-with(myParGrid,mapply(FUN = lifecontingencies:::.fDAxnCpp,T=T,y=y,n=n,i=i,m=m,k=k))

test_that("DAxn", {
  expect_equal(outR,outCpp) 
})

#testin Exn

outR<-with(myParGrid,mapply(FUN = lifecontingencies:::.fExn,T=T,y=y,n=n,i=i))
outCpp<-with(myParGrid,mapply(FUN = lifecontingencies:::.fExnCpp,T=T,y=y,n=n,i=i))

test_that("Exn", {
  expect_equal(outR,outCpp) 
})

#testing AExn

outR<-with(myParGrid,mapply(FUN = lifecontingencies:::.fAExn,T=T,y=y,n=n,i=i,k=k))
outCpp<-with(myParGrid,mapply(FUN = lifecontingencies:::.fAExnCpp,T=T,y=y,n=n,i=i,k=k))

test_that("AExn", {
  expect_equal(outR,outCpp) 
})
