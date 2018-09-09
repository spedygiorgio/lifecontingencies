library(lifecontingencies)

pXt <- Vectorize(lifecontingencies:::pxtold, "x")


# class = "lifetable"
valdezDf<-data.frame(
  x=0:4,
  lx=c(4832555,4821937,4810206,4797185,4782737)
)
valdezltb<-new("lifetable",name="ValdezExample", 
               x=valdezDf$x, lx=valdezDf$lx)

pxt(valdezltb,x=0:2,t=3)
pXt(valdezltb,x=0:2,t=3)

# class = "actuarialtable"
valdezDf<-data.frame(
  x=0:4,
  lx=c(4832555,4821937,4810206,4797185,4782737)
)
valdezact<-new("actuarialtable",name="ValdezExample", 
               x=valdezDf$x, lx=valdezDf$lx, interest=2/100)

pxt(valdezact,x=0:2,t=3)
pXt(valdezact,x=0:2,t=3)

# class = "mdt"
valdezDf<-data.frame(
  x=c(50:54),
  lx=c(4832555,4821937,4810206,4797185,4782737),
  heart=c(5168, 5363, 5618, 5929, 6277),
  accidents=c(1157, 1206, 1443, 1679,2152),
  other=c(4293,5162,5960,6840,7631)
)
valdezMdt<-new("mdt",name="ValdezExample",table=valdezDf)


pxt(valdezMdt,x=50:51,t=3)
pXt(valdezMdt,x=50:51,t=3)

