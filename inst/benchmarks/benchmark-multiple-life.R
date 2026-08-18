library(lifecontingencies)
data(soa08Act)

tables <- list(soa08Act, soa08Act, soa08Act)
pxyzt_old <- function(x, t, ...) {
  sapply(seq_len(NROW(x)), function(i)
    lifecontingencies:::pxyztold(x = x[i, ], t = t[i, 1], ...))
}

x <- cbind(1:120, 11:20, 91:120)
t <- cbind(1:20, 1:20, 1:20)
t <- rbind(t, t, t, t, t, t) + 1/2

for (frac in c("linear", "harm", "exp")) {
  stopifnot(sum(abs(pxyzt(tables, x = x, t = t, fractional = frac) -
                    pxyzt_old(x, t, tablesList = tables, fractional = frac))) < 1e-6)
}

nrep <- 10
rbind(
  pxyzt_new = system.time(replicate(nrep, pxyzt(tables, x = x, t = t))),
  pxyzt_old = system.time(replicate(nrep, pxyzt_old(x, t, tablesList = tables)))
)[, 1:3]
