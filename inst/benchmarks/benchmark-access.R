library(lifecontingencies)
data(soa08Act)

# Development benchmark for mortality-table access patterns from the legacy suite.
f <- function(x) soa08Act@lx[which(soa08Act@x == x)]
g <- function(j) soa08Act@lx[soa08Act@x[j + 2]]

if (soa08Act@x[1] == 0) {
  stopifnot(all(g(1:10) == sapply(1:10, f)))
  nrep <- 1000
  rbind(
    lookup = system.time(replicate(nrep, sapply(0:139, f))),
    indexed = system.time(replicate(nrep, g(0:139)))
  )[, 1:3]
}
