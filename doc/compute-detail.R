## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      collapse = TRUE,
                      comment = "#>")

## -----------------------------------------------------------------------------
library(lifecontingencies)
data("soa08Act")
pXt <- Vectorize(lifecontingencies:::pxtold, "x")
pxT <- Vectorize(lifecontingencies:::pxtold, "t")
pxtvect <- pxt

z <- 1:6/3
#non integer time
cbind(t=z, pxtvect(soa08Act, x=100, t=z, fractional = "lin"), pxT(object=soa08Act, x=100, t=z, fractional = "lin"))
cbind(t=z, pxtvect(soa08Act, x=100, t=z, fractional = "hyp"), pxT(object=soa08Act, x=100, t=z, fractional = "hyp"))
cbind(t=z, pxtvect(soa08Act, x=100, t=z, fractional = "exp"), pxT(object=soa08Act, x=100, t=z, fractional = "exp"))

## -----------------------------------------------------------------------------
x <- 50+0:6/6
#non-integer age
cbind(x=x, pxtvect(soa08Act, x=x, t=1, fractional = "lin"), pXt(object=soa08Act, x=x, t=1, fractional = "lin"))
cbind(x=x, pxtvect(soa08Act, x=x, t=1, fractional = "hyp"), pXt(object=soa08Act, x=x, t=1, fractional = "hyp"))
cbind(x=x, pxtvect(soa08Act, x=x, t=1, fractional = "exp"), pXt(object=soa08Act, x=x, t=1, fractional = "exp"))

## -----------------------------------------------------------------------------
x <- 135:145
#high-age
cbind(x=x, pxtvect(soa08Act, x=x, t=1), pXt(object=soa08Act, x=x, t=1))

