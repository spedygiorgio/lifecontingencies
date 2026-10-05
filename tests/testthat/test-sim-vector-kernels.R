library(testthat)
library(lifecontingencies)

context("Vectorized simulation kernels match the scalar reference")

# Each vectorized kernel .fXCppVec must be numerically equivalent to applying
# the scalar reference element by element for every element of its input.
# These tests pin the kernels (not the end-to-end rLifeContingencies calls,
# which also go through rLife and would be exercised by the existing legacy
# regression tests).

data(soa08Act)

# Helper: build a representative distribution of death-times covering the
# before-, during-, and after-benefit windows plus boundaries.
grid_T <- function(y, n, m = 0, k = 1) {
  step <- 1 / k
  low  <- y + m
  high <- y + m + n - step
  c(
    y - 1,                       # strictly before issue
    low - step / 2,              # just before the start of cover
    low,                         # exact lower boundary
    seq(low, high, length.out = 7),
    high,                        # exact upper boundary
    high + step / 2,             # just after cover
    y + m + n + 10               # well after
  )
}

test_that(".fExnCppVec matches .fExnCpp element-wise", {
  y <- 40; n <- 20; i <- 0.05
  T <- grid_T(y, n)
  expected <- vapply(T, lifecontingencies:::.fExnCpp,
                     y = y, n = n, i = i, FUN.VALUE = numeric(1))
  actual <- lifecontingencies:::.fExnCppVec(T, y = y, n = n, i = i)
  expect_equal(actual, expected, tolerance = 1e-12)
})

test_that(".fAxnCppVec matches .fAxnCpp element-wise (k = 1 and k = 12)", {
  y <- 45; n <- 25; i <- 0.04; m <- 5
  for (k in c(1, 4, 12)) {
    T <- grid_T(y, n, m, k)
    expected <- vapply(T, lifecontingencies:::.fAxnCpp,
                       y = y, n = n, i = i, m = m, k = k,
                       FUN.VALUE = numeric(1))
    actual <- lifecontingencies:::.fAxnCppVec(T, y = y, n = n, i = i,
                                              m = m, k = k)
    expect_equal(actual, expected, tolerance = 1e-12,
                 label = sprintf("k = %g", k))
  }
})

test_that(".fIAxnCppVec and .fDAxnCppVec match their scalar counterparts", {
  y <- 50; n <- 15; i <- 0.03; m <- 0; k <- 1
  T <- grid_T(y, n, m, k)

  exp_i <- vapply(T, lifecontingencies:::.fIAxnCpp,
                  y = y, n = n, i = i, m = m, k = k, FUN.VALUE = numeric(1))
  act_i <- lifecontingencies:::.fIAxnCppVec(T, y = y, n = n, i = i,
                                            m = m, k = k)
  expect_equal(act_i, exp_i, tolerance = 1e-12)

  exp_d <- vapply(T, lifecontingencies:::.fDAxnCpp,
                  y = y, n = n, i = i, m = m, k = k, FUN.VALUE = numeric(1))
  act_d <- lifecontingencies:::.fDAxnCppVec(T, y = y, n = n, i = i,
                                            m = m, k = k)
  expect_equal(act_d, exp_d, tolerance = 1e-12)
})

test_that(".fAExnCppVec matches .fAExnCpp element-wise", {
  y <- 35; n <- 30; i <- 0.045; k <- 2
  T <- grid_T(y, n, 0, k)
  expected <- vapply(T, lifecontingencies:::.fAExnCpp,
                     y = y, n = n, i = i, k = k, FUN.VALUE = numeric(1))
  actual <- lifecontingencies:::.fAExnCppVec(T, y = y, n = n, i = i, k = k)
  expect_equal(actual, expected, tolerance = 1e-12)
})

test_that(".faxnCppVec matches the R .faxn (advance and immediate)", {
  faxn <- getFromNamespace(".faxn", "lifecontingencies")
  y <- 60; n <- 10; i <- 0.03; m <- 0
  for (k in c(1, 12)) {
    for (payment in c("advance", "immediate")) {
      T <- grid_T(y, n, m, k)
      expected <- vapply(T, faxn, y = y, n = n, i = i, m = m, k = k,
                         payment = payment, FUN.VALUE = numeric(1))
      advance <- payment == "advance"
      actual <- lifecontingencies:::.faxnCppVec(T, y = y, n = n, i = i,
                                                m = m, k = k,
                                                advance = advance)
      expect_equal(actual, expected, tolerance = 1e-10,
                   label = sprintf("k = %g, payment = %s", k, payment))
    }
  }
})

test_that(".fAxyznCppVec matches .fAxyzn row-by-row", {
  fAxyzn <- getFromNamespace(".fAxyzn", "lifecontingencies")
  # Build a two-head matrix of representative death times.
  set.seed(42)
  n_rows <- 50
  M <- cbind(
    sample(seq(20, 90, by = 0.5), n_rows, replace = TRUE),
    sample(seq(20, 90, by = 0.5), n_rows, replace = TRUE)
  )
  y <- c(40, 45); n <- 25; i <- 0.04; m <- 0; k <- 1
  for (status in c("joint", "last")) {
    expected <- apply(M, 1, fAxyzn, y = y, n = n, i = i, m = m, k = k,
                      status = status)
    joint <- status == "joint"
    actual <- lifecontingencies:::.fAxyznCppVec(M, y = as.double(y), n = n,
                                                i = i, m = m, k = k,
                                                joint = joint)
    expect_equal(actual, expected, tolerance = 1e-12,
                 label = sprintf("status = %s", status))
  }
})

test_that(".faxyznCppVec matches .faxyzn row-by-row", {
  faxyzn <- getFromNamespace(".faxyzn", "lifecontingencies")
  set.seed(123)
  n_rows <- 50
  M <- cbind(
    sample(seq(20, 90, by = 0.5), n_rows, replace = TRUE),
    sample(seq(20, 90, by = 0.5), n_rows, replace = TRUE)
  )
  y <- c(55, 60); n <- 15; i <- 0.03; m <- 0; k <- 1
  for (status in c("joint", "last")) {
    for (payment in c("advance", "immediate")) {
      expected <- apply(M, 1, faxyzn, y = y, n = n, i = i, m = m, k = k,
                        status = status, payment = payment)
      joint <- status == "joint"
      advance <- payment == "advance"
      actual <- lifecontingencies:::.faxyznCppVec(M, y = as.double(y), n = n,
                                                  i = i, m = m, k = k,
                                                  joint = joint,
                                                  advance = advance)
      expect_equal(actual, expected, tolerance = 1e-10,
                   label = sprintf("status = %s, payment = %s",
                                   status, payment))
    }
  }
})

test_that("rLifeContingencies gives an unbiased estimator (sanity check)", {
  skip_on_cran()
  # Set a seed to keep the Monte-Carlo variance in check without inflating n.
  set.seed(20261004)
  out <- rLifeContingencies(n = 20000, lifecontingency = "Axn",
                            object = soa08Act, x = 40,
                            t = getOmega(soa08Act) - 40, m = 0)
  APV <- Axn(soa08Act, x = 40)
  # 3-sigma band — we only want to catch systematic breakage.
  expect_lt(abs(mean(out) - APV), 3 * sd(out) / sqrt(length(out)))
})

test_that(".resolve_nthreads is opt-in via options(lifecontingencies.openmp)", {
  resolve <- getFromNamespace(".resolve_nthreads", "lifecontingencies")
  old <- options(lifecontingencies.openmp = NULL, lifecontingencies.nthreads = NULL)
  on.exit(options(old), add = TRUE)

  # parallel = FALSE never parallelises, whatever the options say
  options(lifecontingencies.openmp = TRUE)
  expect_identical(resolve(FALSE, 4L), 1L)

  # parallel = TRUE without the option is ignored
  options(lifecontingencies.openmp = NULL)
  expect_identical(resolve(TRUE, 4L), 1L)
  options(lifecontingencies.openmp = FALSE)
  expect_identical(resolve(TRUE, 4L), 1L)

  # with the option, threads follow nthreads / option / default of 2
  # (only meaningful if the binary was built with OpenMP)
  options(lifecontingencies.openmp = TRUE)
  if (lifecontingencies:::.hasOpenMP()) {
    expect_identical(resolve(TRUE, 3L), 3L)
    expect_identical(resolve(TRUE, NULL), 2L)
    options(lifecontingencies.nthreads = 5L)
    expect_identical(resolve(TRUE, NULL), 5L)
    expect_identical(resolve(TRUE, "bad"), 1L)
  } else {
    expect_identical(resolve(TRUE, 3L), 1L)
  }
})

test_that("parallel = TRUE produces the same result as parallel = FALSE", {
  skip_on_cran()
  old <- options(lifecontingencies.openmp = TRUE, lifecontingencies.nthreads = 2L)
  on.exit(options(old), add = TRUE)
  seed <- 20261004L
  set.seed(seed)
  seq_run <- rLifeContingencies(n = 5000, lifecontingency = "Axn",
                                object = soa08Act, x = 50,
                                t = 20, m = 0, parallel = FALSE)
  set.seed(seed)
  par_run <- rLifeContingencies(n = 5000, lifecontingency = "Axn",
                                object = soa08Act, x = 50,
                                t = 20, m = 0, parallel = TRUE)
  # Deaths come from rLife (R-level RNG) so with the same seed the two paths
  # must agree exactly: the parallel flag only affects the payoff computation.
  expect_identical(par_run, seq_run)
})

test_that("rLifeContingencies/Xyz annuities honour payment = advance/arrears end-to-end", {
  # Regression: the wrappers must map payment -> advance flag correctly. Compare
  # against the scalar R reference (getLifecontingencyPv*, which uses .faxn/.faxyzn)
  # on the very same simulated lifetimes.
  for (k in c(1, 12)) for (pay in c("advance", "arrears")) {
    set.seed(11)
    got <- rLifeContingencies(n = 400, lifecontingency = "axn", object = soa08Act,
                              x = 40, t = 20, m = 0, k = k, payment = pay)
    set.seed(11)
    d <- 40 + rLife(n = 400, object = soa08Act, x = 40, k = k,
                    type = if (k == 1) "Kx" else "Tx")
    ref <- getLifecontingencyPv(d, "axn", soa08Act, x = 40, t = 20, m = 0, k = k,
                                payment = pay)
    expect_equal(got, ref, tolerance = 1e-10,
                 label = sprintf("axn k=%g payment=%s", k, pay))
  }
  # advance and arrears must differ (annuity-due > annuity-immediate on average)
  set.seed(3); due <- rLifeContingencies(2000, "axn", soa08Act, x = 40, t = 20, payment = "advance")
  set.seed(3); imm <- rLifeContingencies(2000, "axn", soa08Act, x = 40, t = 20, payment = "arrears")
  expect_gt(mean(due), mean(imm))

  tl <- list(soa08Act, soa08Act)
  for (st in c("joint", "last")) for (pay in c("advance", "arrears")) {
    set.seed(5)
    got <- rLifeContingenciesXyz(n = 300, lifecontingency = "axyz", tablesList = tl,
                                 x = c(50, 55), t = 15, m = 0, k = 1, status = st,
                                 payment = pay)
    set.seed(5)
    d <- c(50, 55)[col(matrix(0, 300, 2))] + rLifexyz(300, tl, x = c(50, 55), k = 1, type = "Kx")
    ref <- apply(d, 1, getFromNamespace(".faxyzn", "lifecontingencies"),
                 y = c(50, 55), n = 15, i = soa08Act@interest, m = 0, k = 1,
                 status = st, payment = pay)
    expect_equal(got, ref, tolerance = 1e-10, label = sprintf("axyz %s %s", st, pay))
  }
})
