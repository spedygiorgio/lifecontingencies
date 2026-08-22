# Performance benchmark for the optimized standard one-life APVs.
# Run from the repository root with:
#   Rscript inst/benchmarks/benchmark-standard-actuarial-optimized.R
#
# The benchmark loads the current checkout, rather than an arbitrary installed
# version of lifecontingencies. This is important when testing a feature branch.

if (!requireNamespace("bench", quietly = TRUE))
  stop("Package 'bench' is required. Install it with install.packages('bench').")
if (!requireNamespace("pkgload", quietly = TRUE))
  stop("Package 'pkgload' is required. Install it with install.packages('pkgload').")

pkgload::load_all(".", quiet = TRUE)
data(soa08Act)

# Scalar legacy references. These intentionally reproduce the historical
# one-observation-at-a-time execution path.
axn_legacy <- function(x, n, m = 0, k = 1, payment = "advance", ...) {
  vapply(seq_along(x), function(j) {
    lifecontingencies:::axnold(
      soa08Act, x = x[j], n = n, m = m, k = k,
      payment = payment, ...
    )
  }, numeric(1))
}

Axn_legacy <- function(x, n, m = 0, k = 1, ...) {
  vapply(seq_along(x), function(j) {
    lifecontingencies:::Axnold(
      soa08Act, x = x[j], n = n, m = m, k = k, ...
    )
  }, numeric(1))
}

AExn_legacy <- function(x, n, k = 1, i = soa08Act@interest, ...) {
  vapply(seq_along(x), function(j) {
    lifecontingencies:::Axnold(
      soa08Act, x = x[j], n = n, i = i, k = k, ...
    ) + Exn(soa08Act, x = x[j], n = n, i = i)
  }, numeric(1))
}

assert_close <- function(new, old, label, tolerance = 1e-10) {
  if (!isTRUE(all.equal(new, old, tolerance = tolerance))) {
    max_error <- max(abs(new - old), na.rm = TRUE)
    stop(sprintf(
      "Correctness check failed for %s (max abs. error = %.17g).",
      label, max_error
    ))
  }
  invisible(TRUE)
}

cat("Checking numerical correctness before benchmarking...\n")
x_check <- c(30, 45, 60, 75)

for (payment in c("advance", "due", "immediate", "arrears")) {
  assert_close(
    axn(soa08Act, x = x_check, n = 20, m = 2, k = 4, payment = payment),
    axn_legacy(soa08Act, x = x_check, n = 20, m = 2, k = 4, payment = payment),
    paste0("axn/payment=", payment)
  )
}

for (k in c(1, 2, 4, 12)) {
  assert_close(
    Axn(soa08Act, x = x_check, n = 20, m = 2, k = k),
    Axn_legacy(soa08Act, x = x_check, n = 20, m = 2, k = k),
    paste0("Axn/k=", k)
  )
  assert_close(
    AExn(soa08Act, x = x_check, n = 20, k = k),
    AExn_legacy(x_check, n = 20, k = k),
    paste0("AExn/k=", k)
  )
}

# Vary x, n and m simultaneously. This specifically tests the optimized
# grouping of deterministic payment/time grids.
x_vec <- c(30, 45, 60, 75)
n_vec <- c(10, 15, 20, 25)
m_vec <- c(0, 1, 2, 3)
assert_close(
  axn(soa08Act, x = x_vec, n = n_vec, m = m_vec, k = 4),
  vapply(seq_along(x_vec), function(j)
    lifecontingencies:::axnold(
      soa08Act, x = x_vec[j], n = n_vec[j], m = m_vec[j], k = 4
    ), numeric(1)),
  "axn/vectorized n,m"
)
assert_close(
  Axn(soa08Act, x = x_vec, n = n_vec, m = m_vec, k = 4),
  vapply(seq_along(x_vec), function(j)
    lifecontingencies:::Axnold(
      soa08Act, x = x_vec[j], n = n_vec[j], m = m_vec[j], k = 4
    ), numeric(1)),
  "Axn/vectorized n,m"
)

# Fractional mortality assumptions must still be passed through pxt/qxt.
for (fractional in c("linear", "constant force", "hyperbolic")) {
  expected <- vapply(x_check, function(xx) {
    times <- seq(0.5, 10, by = 0.5)
    presentValue(
      cashFlows = rep(0.5, length(times)),
      timeIds = times,
      interestRates = soa08Act@interest,
      probabilities = pxt(
        soa08Act, x = xx, t = times, fractional = fractional
      )
    )
  }, numeric(1))
  assert_close(
    axn(soa08Act, x = x_check, n = 10, k = 2, payment = "immediate",
        fractional = fractional),
    expected,
    paste0("axn/fractional=", fractional)
  )
}

cat("All correctness checks passed.\n\n")

cases <- list(
  scalar = 65,
  ages_100 = 1:100,
  ages_1000 = seq(20, 90, length.out = 1000),
  ages_5000 = seq(20, 90, length.out = 5000)
)

benchmark_one <- function(case_name, function_name, x, new_expr, old_expr,
                          iterations) {
  b <- bench::mark(
    optimized = eval(new_expr),
    legacy = eval(old_expr),
    iterations = iterations,
    check = FALSE,
    memory = TRUE,
    time_unit = "ms"
  )
  b$case <- case_name
  b$function_name <- function_name
  b
}

run_case <- function(case_name, x, iterations) {
  rbind(
    benchmark_one(
      case_name, "axn", x,
      quote(axn(soa08Act, x = x, n = 20, k = 4)),
      quote(axn_legacy(x, n = 20, k = 4)),
      iterations
    ),
    benchmark_one(
      case_name, "Axn", x,
      quote(Axn(soa08Act, x = x, n = 20, k = 4)),
      quote(Axn_legacy(x, n = 20, k = 4)),
      iterations
    ),
    benchmark_one(
      case_name, "AExn", x,
      quote(AExn(soa08Act, x = x, n = 20, k = 4)),
      quote(AExn_legacy(x, n = 20, k = 4)),
      iterations
    )
  )
}

raw <- do.call(rbind, lapply(names(cases), function(case_name) {
  x <- cases[[case_name]]
  iterations <- if (length(x) >= 1000) 3L else 5L
  run_case(case_name, x, iterations)
}))

raw$median_ms <- as.numeric(raw$median)
raw$itr_per_sec <- as.numeric(raw$`itr/sec`)
raw$memory_mb <- as.numeric(raw$mem_alloc) / 1024^2

summary <- do.call(rbind, lapply(split(raw,
                                      list(raw$case, raw$function_name),
                                      drop = TRUE), function(z) {
  opt <- z[z$expression == "optimized", ][1, ]
  old <- z[z$expression == "legacy", ][1, ]
  data.frame(
    case = opt$case,
    function_name = opt$function_name,
    optimized_median_ms = opt$median_ms,
    legacy_median_ms = old$median_ms,
    speedup = old$median_ms / opt$median_ms,
    optimized_itr_sec = opt$itr_per_sec,
    legacy_itr_sec = old$itr_per_sec,
    optimized_memory_mb = opt$memory_mb,
    legacy_memory_mb = old$memory_mb,
    row.names = NULL
  )
}))

cat("Performance results (speedup = legacy / optimized):\n")
print(summary, row.names = FALSE)
cat("\nThe full bench::mark result is stored in object 'raw'.\n")
