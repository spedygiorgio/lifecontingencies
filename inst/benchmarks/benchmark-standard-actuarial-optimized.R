library(lifecontingencies)

# Development benchmark: compare the optimized public implementations against
# the scalar legacy kernels. This script is intentionally not run by testthat.
data(soa08Act)

axn_legacy <- Vectorize(lifecontingencies:::axnold, "x")
Axn_legacy <- Vectorize(lifecontingencies:::Axnold, "x")
AExn_legacy <- function(x, n, k = 1, i = soa08Act@interest) {
  vapply(x, function(xx) {
    lifecontingencies:::Axnold(soa08Act, x = xx, n = n, i = i, k = k) +
      Exn(soa08Act, x = xx, n = n, i = i)
  }, numeric(1))
}

median_time <- function(expr_fun, reps = 20L) {
  z <- replicate(reps, system.time(expr_fun())["elapsed"])
  unname(median(z))
}

benchmark_case <- function(label, expr_new, expr_old, reps = 20L) {
  new_time <- median_time(expr_new, reps)
  old_time <- median_time(expr_old, reps)
  data.frame(
    function = label,
    new_seconds = new_time,
    legacy_seconds = old_time,
    speedup = old_time / new_time,
    row.names = NULL
  )
}

cases <- list(
  scalar = 65,
  ages_100 = 1:100,
  ages_1000 = seq(20, 90, length.out = 1000),
  ages_10000 = seq(20, 90, length.out = 10000)
)

results <- do.call(rbind, lapply(names(cases), function(size_name) {
  x <- cases[[size_name]]
  rbind(
    cbind(case = size_name,
          benchmark_case("axn",
                         function() axn(soa08Act, x = x, n = 20, k = 4),
                         function() axn_legacy(soa08Act, x = x, n = 20, k = 4))),
    cbind(case = size_name,
          benchmark_case("Axn",
                         function() Axn(soa08Act, x = x, n = 20, k = 4),
                         function() Axn_legacy(soa08Act, x = x, n = 20, k = 4))),
    cbind(case = size_name,
          benchmark_case("AExn",
                         function() AExn(soa08Act, x = x, n = 20, k = 4),
                         function() AExn_legacy(x, n = 20, k = 4)))
  )
}))

print(results)

# Correctness spot checks are deliberately included in the benchmark so that
# a very fast but numerically incorrect implementation is immediately visible.
stopifnot(isTRUE(all.equal(
  axn(soa08Act, x = 1:100, n = 20, k = 4),
  axn_legacy(soa08Act, x = 1:100, n = 20, k = 4),
  tolerance = 1e-10
)))
stopifnot(isTRUE(all.equal(
  Axn(soa08Act, x = 1:100, n = 20, k = 4),
  Axn_legacy(soa08Act, x = 1:100, n = 20, k = 4),
  tolerance = 1e-10
)))
