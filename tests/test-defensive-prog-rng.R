require(lifecontingencies)


#test abbreviated arguments
data(soa08Act)
set.seed(123)
rLife(2, soa08Act, x=0,k=1, type="T")
set.seed(123)
rLife(2, soa08Act, x=0,k=1, type="cont")

set.seed(123)
rLife(2, soa08Act, x=0,k=1, type="Kx")
set.seed(123)
rLife(2, soa08Act, x=0,k=1, type="cur")

rLifexyz(3, list(soa08Act, soa08Act), x=33:34, k=1, type="cont")

set.seed(123)
rLifeContingenciesXyz(n=5, lifecontingency = "Axyz", tablesList = list(soa08Act, soa08Act), 
                      x=59:60, t=30, m=0, k=1, status="j")
set.seed(123)
rLifeContingenciesXyz(n=5, lifecontingency = "Axyz", tablesList = list(soa08Act, soa08Act), 
                      x=59:60, t=30, m=0, k=1, status="joint")

getLifecontingencyPvXyz(deathsTimeXyz=matrix(c(50,50,51,43,44,22,12,56,20,24,53,12),ncol=2),
  lifecontingency = "axyz", tablesList = list(soa08Act, soa08Act), i = 0.03, t=30,x=c(40,50),
  m=0, k=1,status="la")


#test defensive programming 
try(
  rLifexyz(3, list(soa08Act, soa08Act), x=33:34, k=1, type="toto")
)

