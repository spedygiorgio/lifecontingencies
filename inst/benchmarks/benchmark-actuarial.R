library(lifecontingencies)
data(soa08Act)

# Legacy actuarial accuracy comparisons and performance measurements.
# These are development benchmarks and are intentionally not run by testthat.
axn_old_x <- Vectorize(lifecontingencies:::axnold, "x")
axn_old_n <- Vectorize(lifecontingencies:::axnold, "n")
Axn_old_x <- Vectorize(lifecontingencies:::Axnold, "x")
Axn_old_n <- Vectorize(lifecontingencies:::Axnold, "n")

stopifnot(all.equal(axn(soa08Act, x = 1:100, payment = "advance"),
                    axn_old_x(soa08Act, x = 1:100, payment = "advance")))
stopifnot(all.equal(Axn(soa08Act, x = 1:100), Axn_old_x(soa08Act, x = 1:100)))

nrep <- 10
rbind(
  axn_new = system.time(replicate(nrep, axn(soa08Act, x = 1:100, payment = "advance"))),
  axn_old = system.time(replicate(nrep, axn_old_x(soa08Act, x = 1:100, payment = "advance"))),
  Axn_new = system.time(replicate(nrep, Axn(soa08Act, x = 1:100))),
  Axn_old = system.time(replicate(nrep, Axn_old_x(soa08Act, x = 1:100)))
)[, 1:3]
