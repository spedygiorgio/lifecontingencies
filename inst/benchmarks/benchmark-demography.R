library(lifecontingencies)
data(soa08Act)

# Legacy demographic performance comparisons.
pXt_old <- Vectorize(lifecontingencies:::pxtold, "x")
pxT_old <- Vectorize(lifecontingencies:::pxtold, "t")

for (frac in c("linear", "harm", "exp")) {
  ages <- seq(1, 100, by = 1/4)
  times <- seq(1, 30, by = 1/4)
  stopifnot(sum(abs(pxt(soa08Act, x = ages, t = 1/3, fractional = frac) -
                    pXt_old(soa08Act, x = ages, t = 1/3, fractional = frac))) < 1e-6)
  stopifnot(sum(abs(pxt(soa08Act, x = 2, t = times, fractional = frac) -
                    pxT_old(soa08Act, x = 2, t = times, fractional = frac))) < 1e-6)
}

nrep <- 10
rbind(
  pxt_new_x = system.time(replicate(nrep, pxt(soa08Act, x = 1:130, t = 1/2))),
  pxt_old_x = system.time(replicate(nrep, pXt_old(soa08Act, x = 1:130, t = 1/2))),
  pxt_new_t = system.time(replicate(nrep, pxt(soa08Act, x = 1, t = 1:130/2))),
  pxt_old_t = system.time(replicate(nrep, pxT_old(soa08Act, x = 1, t = 1:130/2)))
)[, 1:3]
