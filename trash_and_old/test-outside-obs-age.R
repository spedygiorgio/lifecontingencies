library(lifecontingencies)


data(soaLt)
soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
                         x=x,lx=Ix,name="SOA2008"))
#evaluate and life-long annuity for an aged 65
axn(soa08Act, x=110) 
axn(soa08Act, x=112) 
axn(soa08Act, x=112, n=1) 

curve(axn(soa08Act, x), from=0, to=110, ylab="ax")
curve(axn(soa08Act, x, n=10), col="red", add=TRUE)

Axn(soa08Act, x=112) 
Axn(soa08Act, x=112, n=1) 
