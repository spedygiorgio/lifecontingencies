require(lifecontingencies)

data(soa08Act)

#test abbreviated arguments
axn(soa08Act, x=90, type="E")
axn(soa08Act, x=90, type="exp")

axn(soa08Act, x=90, type="S", payment = "adv")
axn(soa08Act, x=90, type="sto", payment  = "arr")

Iaxn(soa08Act, x=90, type="E")


Axn(soa08Act, x=90, type="E")
IAxn(soa08Act, x=90, type="E")
DAxn(soa08Act, x=90, type="E")

Exn(soa08Act, x=90, n=5, type="E")

axyzn(list(soa08Act, soa08Act, soa08Act), x=90:88, type="E")
Axyzn(list(soa08Act, soa08Act, soa08Act), x=90:88, type="E")


axyzn(list(soa08Act, soa08Act), x=60:61, status="j")
axyzn(list(soa08Act, soa08Act), x=60:61, status="l")

Axyzn(list(soa08Act, soa08Act), x=60:61, status="j")
Axyzn(list(soa08Act, soa08Act), x=60:61, status="l")


#test defensive programming 
try(
  axn(soa08Act, x=90, type="foo1")
)

