library(lifecontingencies)

if (!requireNamespace("bench", quietly = TRUE)) {
  stop("Package 'bench' is required to run this development benchmark.")
}

data("soa08Act")

# Benchmark the native pxt kernel against an independent R implementation of
# the same calculation. This deliberately bypasses the public pxt() wrapper
# so that the benchmark measures the computational kernel rather than wrapper
# overhead.
reference_pxt_kernel <- function(x, t, lx, omega, fractional_method) {
  nx <- length(x)
  nt <- length(t)
  n <- max(nx, nt)

  if (nx == 0 || nt == 0) {
    return(numeric(0))
  }

  x <- rep(x, length.out = n)
  t <- rep(t, length.out = n)
  out <- numeric(n)

  get_lx <- function(age) {
    if (age < 0 || age > omega || age == omega + 1) {
      return(0)
    }
    lx[age + 1L]
  }

  for (i in seq_len(n)) {
    xi <- x[i]
    ti <- t[i]

    if (xi < 0 || ti < 0) {
      stop("Check x or t domain")
    }

    floor_x <- floor(xi)
    eps_x <- xi - floor_x
    u <- ti + eps_x
    floor_u <- floor(u)
    eps_u <- u - floor_u

    ix <- as.integer(floor_x)
    ix1 <- ix + 1L
    ixu <- ix + as.integer(floor_u)
    ixu1 <- ixu + 1L

    lfx <- get_lx(ix)
    if (lfx == 0) {
      out[i] <- 0
      next
    }

    lfx1 <- get_lx(ix1)
    lfxu <- get_lx(ixu)
    lfxu1 <- get_lx(ixu1)

    floor_u_p_floor_x <- lfxu / lfx
    one_p_floor_xu <- if (lfxu == 0) 0 else lfxu1 / lfxu
    one_p_floor_x <- lfx1 / lfx

    if (fractional_method == 0L) {
      u_p_floor_x <- floor_u_p_floor_x *
        (1 - eps_u * (1 - one_p_floor_xu))
    } else if (fractional_method == 1L) {
      u_p_floor_x <- floor_u_p_floor_x *
        one_p_floor_xu^eps_u
    } else {
      u_p_floor_x <- floor_u_p_floor_x * one_p_floor_xu /
        (1 - (1 - eps_u) * (1 - one_p_floor_xu))
    }

    if (fractional_method == 0L) {
      eps_x_p_floor_x <- 1 - eps_x * (1 - one_p_floor_x)
    } else if (fractional_method == 1L) {
      eps_x_p_floor_x <- one_p_floor_x^eps_x
    } else {
      eps_x_p_floor_x <- one_p_floor_x /
        (1 - (1 - eps_x) * (1 - one_p_floor_x))
    }

    out[i] <- u_p_floor_x / eps_x_p_floor_x
  }

  out
}

set.seed(2026)

lx <- soa08Act@lx
omega <- getOmega(soa08Act)

benchmark_pxt_case <- function(n, fractional_method, scalar_x = FALSE,
                               scalar_t = FALSE) {
  x <- if (scalar_x) 40.5 else runif(n, 20, 80.5)
  t <- if (scalar_t) 5.25 else runif(n, 0.25, 10.75)

  expected <- reference_pxt_kernel(
    x, t, lx, omega, fractional_method
  )
  actual <- lifecontingencies:::.pxtCpp(
    x, t, lx, omega, fractional_method
  )
  stopifnot(all.equal(actual, expected, tolerance = 1e-12))

  timing <- bench::mark(
    R = reference_pxt_kernel(x, t, lx, omega, fractional_method),
    Rcpp = lifecontingencies:::.pxtCpp(x, t, lx, omega, fractional_method),
    iterations = 30,
    check = FALSE,
    time_unit = "ms"
  )

  r_median <- as.numeric(timing$median[[1]])
  cpp_median <- as.numeric(timing$median[[2]])

  data.frame(
    n = n,
    fractional_method = fractional_method,
    scalar_x = scalar_x,
    scalar_t = scalar_t,
    iterations = 30,
    r_median_ms = r_median,
    cpp_median_ms = cpp_median,
    speedup = r_median / cpp_median
  )
}

pxt_results <- do.call(rbind, c(
  lapply(c(100, 1000, 10000, 100000), benchmark_pxt_case,
         fractional_method = 0L),
  lapply(c(100, 1000, 10000, 100000), benchmark_pxt_case,
         fractional_method = 1L),
  lapply(c(100, 1000, 10000, 100000), benchmark_pxt_case,
         fractional_method = 2L),
  lapply(c(1000, 10000, 100000), benchmark_pxt_case,
         fractional_method = 0L, scalar_x = TRUE, scalar_t = TRUE),
  lapply(c(1000, 10000, 100000), benchmark_pxt_case,
         fractional_method = 1L, scalar_x = TRUE, scalar_t = TRUE),
  lapply(c(1000, 10000, 100000), benchmark_pxt_case,
         fractional_method = 2L, scalar_x = TRUE, scalar_t = TRUE)
))

cat("\npxtCpp benchmark:\n")
print(pxt_results, row.names = FALSE)

cat("\npxtCpp summary (n >= 10000):\n")
summary_rows <- pxt_results[pxt_results$n >= 10000, ]
print(summary_rows[, c("n", "fractional_method", "scalar_x", "scalar_t",
                       "r_median_ms", "cpp_median_ms", "speedup")],
      row.names = FALSE)
