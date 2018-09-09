library(lifecontingencies)



axyznvect <- lifecontingencies:::axyznvect



#(tablesList, x, n, i, m, k = 1, status = "joint", type = "EV", power =1, payment = "advance", ...)
  

data(soaLt)
soa08Act <- with(soaLt, new("actuarialtable", interest=0.06, x=x, lx=Ix, name="SOA2008"))
#evaluate and life-long annuity for an aged 65
listSOA <- list(soa08Act, soa08Act, soa08Act) 

checkvector <- function()
{
  sapply(10:90, function(y)
    axyznvect(listSOA, x=c(y, y+1, y+2)) == axyzn(listSOA, x=c(y, y+1, y+2))
  )
}

checkvector()
