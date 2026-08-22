# Performance benchmark for the optimized standard one-life APVs.
# Run from the repository root with:
#   Rscript inst/benchmarks/benchmark-standard-actuarial-optimized.R
#
# The benchmark loads the current checkout, rather than an arbitrary installed
# version of lifecontingencies.

if (!requireNamespace("bench", quietly = TRUE))
  stop("Package 'bench' is required. Install it with install.packages('bench').")
if (!requireNamespace("pkgload", quietly = TRUE))
  stop("Package 'pkgload' is required. Install it with install.packages('pkgload').")

pkgload::load_all(".", quiet = TRUE)
data(soa08Act)
interest <- as.numeric(soa08Act@interest)

# Historical scalar references. These deliberately execute one contract at a
# time, so that the benchmark measures the benefit of vectorized orchestration
# in the optimized public functions.
axn_legacy <- function(x, n, m = 0, k = 1, payment = "advance", ...) {
  ntot <- max(length(x), length(n), length(m))
  x <- rep(x, length.out = ntot)
  n <- rep(n, length.out = ntot)
  m <- rep(m, length.out = ntot)
  vapply(seq_len(ntot), function(j) {
    lifecontingencies:::axnold(
      soa08Act, x = x[j], n = n[j], m = m[j], k = k,
      i = interest, payment = payment, ...
    )
  }, numeric(1))
}

Axn_legacy <- function(x, n, m = 0, k = 1, ...) {
  ntot <- max(length(x), length(n), length(m))
  x <- rep(x, length.out = ntot)
  n <- rep(n, length.out = ntot)
  m <- rep(m, length.out = ntot)
  vapply(seq_len(ntot), function(j) {
    lifecontingencies:::Axnold(
      soa08Act, x = x[j], n = n[j], m = m[j], k = k,
      i = interest, ...
    )
  }, numeric(1))
}

AExn_legacy <- function(x, n, k = 1, ...) {
  ntot <- max(length(x), length(n))
  x <- rep(x, length.out = ntot)
  n <- rep(n, length.out = ntot)
  vapply(seq_len(ntot), function(j) {
    lifecontingencies:::Axnold(
      soa08Act, x = x[j], n = n[j], i = interest, k = k, ...
    ) + Exn(soa08Act, x = x[j], n = n[j], i = interest)
  }, numeric(1))
}

assert_close <- function(new, old, label, tolerance = 1e-10) {
  if (length(new) != length(old)) {
    stop(sprintf("Correctness check failed for %s: different output lengths.", label))
  }
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
n_check <- rep(20, length(x_check))
m_check <- rep(2, length(x_check))

# Explicit i is intentional: it makes the benchmark independent of the
# actuarialtable default-interest dispatch and tests the same numerical input
# in optimized and legacy paths.
for (payment in c("advance", "due", "immediate", "arrears")) {
  assert_close(
    axn(soa08Act, x = x_check, n = n_check, i = interest, m = m_check,
        k = 4, payment = payment),
    axn_legacy(x_check, n = n_check, m = m_check, k = 4,
               payment = payment),
    paste0("axn/payment=", payment)
  )
}

for (k in c(1, 2, 4, 12)) {
  assert_close(
    Axn(soa08Act, x = x_check, n = n_check, i = interest, m = m_check, k = k),
    Axn_legacy(x_check, n = n_check, m = m_check, k = k),
    paste0("Axn/k=", k)
  )
  assert_close(
    AExn(soa08Act, x = x_check, n = n_check, i = interest, k = k),
    AExn_legacy(x_check, n = n_check, k = k),
    paste0("AExn/k=", k)
  )
}

# Vary x, n and m simultaneously. This tests the vectorized parameter path.
x_vec <- c(30, 45, 60, 75)
n_vec <- c(10, 15, 20, 25)
m_vec <- c(0, 1, 2, 3)
assert_close(
  axn(soa08Act, x = x_vec, n = n_vec, i = interest, m = m_vec, k = 4),
  axn_legacy(x_vec, n = n_vec, m = m_vec, k = 4),
  "axn/vectorized n,m"
)
assert_close(
  Axn(soa08Act, x = x_vec, n = n_vec, i = interest, m = m_vec, k = 4),
  Axn_legacy(x_vec, n = n_vec, m = m_vec, k = 4),
  "Axn/vectorized n,m"
)

# Explicit recycling is tested separately from the performance checks.
assert_close(
  axn(soa08Act, x = x_check, n = 20, i = interest, m = 2, k = 4),
  axn_legacy(x_check, n = 20, m = 2, k = 4),
  "axn/scalar n,m recycling"
)

# Fractional mortality assumptions must still pass through pxt/qxt.
for (fractional in c("linear", "constant force", "hyperbolic")) {
  expected_axn <- vapply(seq_along(x_check), function(j) {
    times <- seq(0.5, 10, by = 0.5)
    presentValue(
      cashFlows = rep(0.5, length(times)),
      timeIds = times,
      interestRates = interest,
      probabilities = pxt(
        soa08Act, x = x_check[j], t = times, fractional = fractional
      )
    )
  }, numeric(1))
  assert_close(
    axn(soa08Act, x = x_check, n = rep(10, length(x_check)),
        i = interest, k = 2, payment = "immediate", fractional = fractional),
    expected_axn,
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

benchmark_one <- function(case_name, function_name, x, new_fun, old_fun,
                          iterations) {
  b <- bench::mark(
    optimized = new_fun(),
    legacy = old_fun(),
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
      function() axn(soa08Act, x = x, n = 20, i = interest, k = 4),
      function() axn_legacy(x, n = 20, k = 4),
      iterations
    ),
    benchmark_one(
      case_name, "Axn", x,
      function() Axn(soa08Act, x = x, n = 20, i = interest, k = 4),
      function() Axn_legacy(x, n = 20, k = 4),
      iterations
    ),
    benchmark_one(
      case_name, "AExn", x,
      function() AExn(soa08Act, x = x, n = 20, i = interest, k = 4),
      function() AExn_legacy(x, n = 20, k = 4),
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
