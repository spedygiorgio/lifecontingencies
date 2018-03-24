require(lifecontingencies)

data(soa08Act)

#test abbreviated arguments
pxt(object=soa08Act, x=90, t=3/2, frac="const")
pxt(object=soa08Act, x=90, t=3/2, frac="exp")

pxt(object=soa08Act, x=90, t=3/2, frac="lin")
pxt(object=soa08Act, x=90, t=3/2, frac="unif")

pxt(object=soa08Act, x=90, t=3/2, frac="hyp")
pxt(object=soa08Act, x=90, t=3/2, frac="Bal")
pxt(object=soa08Act, x=90, t=3/2, frac="harm")

qxt(object=soa08Act, x=90, t=3/2, frac="const")
qxt(object=soa08Act, x=90, t=3/2, frac="exp")

qxt(object=soa08Act, x=90, t=3/2, frac="lin")
qxt(object=soa08Act, x=90, t=3/2, frac="unif")

qxt(object=soa08Act, x=90, t=3/2, frac="hyp")
qxt(object=soa08Act, x=90, t=3/2, frac="Bal")
qxt(object=soa08Act, x=90, t=3/2, frac="harm")

pxyzt(list(soa08Act, soa08Act), x=c(55, 50), t=10.33, frac="lin")
pxyzt(list(soa08Act, soa08Act, soa08Act), x=c(55, 50, 45), t=10.33, frac=c("unif", "Bal", "exp"))
pxyzt(list(soa08Act, soa08Act), x=c(55, 50), t=10.33, status="jo")
pxyzt(list(soa08Act, soa08Act), x=c(55, 50), t=10.33, status="Last-Survi")
pxyzt(list(soa08Act, soa08Act), x=c(55, 50), t=10.33, status="la")


exyzt(list(soa08Act, soa08Act), x=c(55, 50), t=10.33, status="la", type="K")
exyzt(list(soa08Act, soa08Act), x=c(55, 50), t=10.33, status="jo", type="T")

#test defensive programming 
try(
pxt(object=soa08Act, x=90, t=2, frac="foo1")
)
try(
pxt(object=soa08Act, x=90, t=2, frac=TRUE)
)

try(
  qxt(object=soa08Act, x=90, t=2, frac="foo1")
)
try(
  qxt(object=soa08Act, x=90, t=2, frac=TRUE)
)
try(
  pxyzt(list(soa08Act, soa08Act, soa08Act), x=c(55, 50, 45), t=10.33, frac=c("foo1", "Bal", "exp"))
)
try(
  pxyzt(list(soa08Act, soa08Act, soa08Act), x=c(55, 50, 45), t=10.33, frac=c("unif", FALSE, "exp"))
)
try(
  pxyzt(list(soa08Act, soa08Act), x=c(55, 50), t=10.33, status="foo2")
)
try(
  pxyzt(list(soa08Act, soa08Act), x=c(55, 50), t=10.33, status=log(3))
)
try(
exyzt(list(soa08Act, soa08Act), x=c(55, 50), t=10.33, status="lol", type="K")
)
try(
exyzt(list(soa08Act, soa08Act), x=c(55, 50), t=10.33, status="jo", type=12345)
)
