library(testthat)
library(lifecontingencies)

context("Native kernels: memory-safety and undefined-behaviour regressions")

# These cases were found with an AddressSanitizer/UBSan build of the package
# (see NEWS). They must neither crash nor read out of bounds, and must give
# sensible results.

data(soa08Act)
ns <- function(nm) getFromNamespace(nm, "lifecontingencies")

test_that("multi-life vector kernels reject a matrix with no columns (was a null dereference)", {
  M0 <- matrix(0, 5, 0)
  expect_error(ns(".fAxyznCppVec")(M0, numeric(0), 10, 0.03, 0, 1, TRUE, 1L),
               "at least one column")
  expect_error(ns(".faxyznCppVec")(M0, numeric(0), 10, 0.03, 0, 1, TRUE, TRUE, 1L),
               "at least one column")
  expect_error(ns(".fAxyznCppVec")(matrix(1, 5, 2), 1, 10, 0.03, 0, 1, TRUE, 1L),
               "length matching")
  expect_length(ns(".fAxyznCppVec")(matrix(0, 0, 2), c(1, 2), 10, 0.03, 0, 1, TRUE, 1L), 0)
})

test_that("annuity kernel handles unbounded term / extreme frequency without integer overflow", {
  faxn <- ns(".faxnCppVec")
  # whole-life annuity-due, k = 1, T = Inf (survives forever): sum v^j = (1+i)/i
  expect_equal(faxn(Inf, 40, Inf, 0.03, 0, 1, TRUE, 1L), 1.03 / 0.03, tolerance = 1e-12)
  # annuity-immediate: sum_{j>=1} v^j = 1/i
  expect_equal(faxn(Inf, 40, Inf, 0.03, 0, 1, FALSE, 1L), 1 / 0.03, tolerance = 1e-12)
  # extreme frequency must stay finite, non-negative and close to the continuous value
  v <- faxn(60, 40, 10, 0.03, 0, 1e300, TRUE, 1L)
  expect_true(is.finite(v) && v > 0)
  expect_equal(v, (1 - 1.03^-10) / log(1.03), tolerance = 1e-6)
})

test_that("annuity kernel propagates NA/NaN lifetimes instead of treating them as survivors", {
  r <- ns(".faxnCppVec")(c(NaN, NA, 50), 40, 10, 0.03, 0, 1, TRUE, 1L)
  expect_true(is.na(r[1]) && is.na(r[2]) && is.finite(r[3]))
})

test_that("pxtCpp is bounds-safe for out-of-range ages and for lx shorter than omega + 1", {
  lx <- as.numeric(300:1)                    # large enough to live on the malloc heap
  expect_equal(ns(".pxtCpp")(c(500, 900), 1, lx, 1000, 0L), c(0, 0))   # was a heap over-read
  expect_equal(ns(".pxtCpp")(1e10, 1, lx, 100, 0L), 0)                  # was double -> int UB
  expect_equal(ns(".pxtCpp")(10, 3e9, lx, 100, 0L), 0)
  expect_equal(ns(".pxtCpp")(NaN, 1, lx, 100, 0L), 0)
})

test_that("pxtLifetableCpp is bounds-safe for NaN/Inf/huge ages and an empty lx", {
  lx <- as.numeric(300:1)
  r <- ns(".pxtLifetableCpp")(c(NaN, 1e10, Inf, -3), 1, lx, 0, 0L)
  expect_length(r, 4)
  expect_length(ns(".pxtLifetableCpp")(10, 1, numeric(0), 0, 1L), 1)
})

test_that("rLifeContingenciesXyz is unbiased when the lives have different issue ages", {
  skip_on_cran()
  # Regression: x was recycled down the column-major matrix of simulated
  # lifetimes, so ages were mixed across lives whenever they differed.
  tl <- list(soa08Act, soa08Act); x <- c(40, 60); n <- 20
  for (st in c("joint", "last")) {
    apv <- Axyzn(tablesList = tl, x = x, n = n, status = st, type = "EV")
    set.seed(1)
    s <- rLifeContingenciesXyz(n = 1e5, lifecontingency = "Axyz", tablesList = tl,
                               x = x, t = n, status = st)
    z <- (mean(s) - apv) / (sd(s) / sqrt(length(s)))
    expect_lt(abs(z), 4, label = paste("z-score,", st))
  }
})
