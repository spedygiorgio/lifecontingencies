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


#test defensive programming 
try(
  rLifexyz(3, list(soa08Act, soa08Act), x=33:34, k=1, type="toto")
)

