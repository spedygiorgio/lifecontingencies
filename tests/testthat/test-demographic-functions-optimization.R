library(testthat)
library(lifecontingencies)

context("Demographic functions optimization (Tx, probs2lifetable, exn Kx branch, exyzt)")

## NOTE: on this branch exn(type="Kx") and exyzt() were already vectorised
## separately (see git history around 2026-09). The tests below still cover
## them as black-box regression/consistency checks -- they exercise
## whichever implementation is currently in R/3_demographicFunctions.R,
## they do not assume a particular internal implementation. Only Tx() and
## probs2lifetable() are the functions actually changed by this patch.

## ---------------------------------------------------------------------
## Test data: real, publicly documented tables.
##  - soa08Act: SOA illustrative actuarial table bundled with the package
##    and used in the function's own @examples (man/other-demographic-
##    functions.Rd: `Tx(soa08Act, 67)`) and in the pre-existing regression
##    test below (tests/testthat/testDemography.R).
##  - ips55M: the Italian IPS55 male projected mortality table, built
##    exactly as shown in the package vignette
##    (vignettes/an_introduction_to_lifecontingencies_package.Rnw,
##    "fromDataFrame2" chunk), where it feeds a documented
##    `exn(ips55M, 50, 20, "complete")` example.
##  - AF92Lt / AM92Lt: UK institute/faculty AF92/AM92 tables bundled with
##    the package.
##  - usaMale07Lt: US Social Security 2007 male table, built exactly as
##    shown in the same vignette ("fromDataFrame1" chunk).
## ---------------------------------------------------------------------

data(soa08Act)
data(demoIta)
data(AF92Lt)
data(AM92Lt)
data(demoUsa)

lxIPS55M <- with(demoIta, IPS55M)
lxIPS55M <- lxIPS55M[!(lxIPS55M %in% c(0, NA))]
xIPS55M <- seq(0, length(lxIPS55M) - 1, 1)
ips55M <- new("lifetable", x = xIPS55M, lx = lxIPS55M, name = "IPS 55 Males")

usaMale07 <- demoUsa[, c("age", "USSS2007M")]
names(usaMale07) <- c("x", "lx")
usaMale07 <- usaMale07[!(usaMale07$lx %in% c(0, NA)), ]
usaMale07Lt <- as(usaMale07, "lifetable")

## ---------------------------------------------------------------------
## Tx(): regression values captured from real tables (documented example
## Tx(soa08Act, 67), plus other bundled/vignette tables and edge ages).
## ---------------------------------------------------------------------

test_that("Tx matches known reference values on real tables", {
  # documented example: man/other-demographic-functions.Rd -> Tx(soa08Act, 67)
  expect_equal(Tx(soa08Act, 67), 1021989.350698633, tolerance = 1e-6)
  expect_equal(Tx(soa08Act, 0), 7180788.511223633, tolerance = 1e-6)
  expect_equal(Tx(soa08Act, 100), 705.722511633, tolerance = 1e-6)
  expect_equal(Tx(ips55M, 50), 3534699.165, tolerance = 1e-6)
  expect_equal(Tx(AF92Lt, 65), 2619076.545803359, tolerance = 1e-6)
  expect_equal(Tx(AM92Lt, 65), 1882921.739122211, tolerance = 1e-6)
  expect_equal(Tx(usaMale07Lt, 40), 3614844.5, tolerance = 1e-6)
})

test_that("Tx is consistent with its own definition (sum of Lxt) and with edge ages", {
  # Tx(x) must equal the sum of one-year Lxt(k,1) terms for k = x..omega,
  # independently recomputed here (not by calling Lxt in a loop, to avoid
  # exercising exactly the code path being optimized away in Tx()).
  for (tb in list(soa08Act, ips55M, AF92Lt, usaMale07Lt)) {
    omega <- getOmega(tb)
    x0 <- tb@x[which(tb@x >= omega - 5)][1]
    idx <- which(tb@x >= x0 & tb@x <= omega)
    lxRange <- tb@lx[idx]
    dxRange <- lxRange - c(lxRange[-1], 0)
    manualLx <- lxRange - 0.5 * dxRange
    expect_equal(Tx(tb, x0), sum(manualLx), tolerance = 1e-8)
  }

  # at the last tabulated age, Tx must reduce to the last Lxt term
  omega <- getOmega(soa08Act)
  expect_equal(Tx(soa08Act, omega), 0)

  # Tx must be strictly decreasing in x (fewer years left to live as x grows)
  ages <- c(0, 20, 40, 60, 80, 100)
  txValues <- sapply(ages, function(a) Tx(soa08Act, a))
  expect_true(all(diff(txValues) < 0))
})

## ---------------------------------------------------------------------
## exn() with type = "Kx" (the vectorised branch) and its default alias
## "curtate", which testtypelifearg() maps onto "Kx".
## ---------------------------------------------------------------------

test_that("exn (Kx / curtate) matches known reference values on real tables", {
  # pre-existing package regression value (tests/testthat/testDemography.R)
  expect_equal(round(exn(soa08Act, x = 70, n = 2), 5), 1.89858)

  # documented example: man/exn.Rd -> exn(object = soa08Act, x = 0)
  expect_equal(exn(soa08Act, x = 0), 71.307885112166, tolerance = 1e-6)

  expect_equal(exn(ips55M, x = 50), 35.802610682812, tolerance = 1e-6)
  expect_equal(exn(ips55M, x = 50, n = 20), 19.433217384464, tolerance = 1e-6)
  expect_equal(exn(AF92Lt, x = 65, n = 10), 9.667536010103, tolerance = 1e-6)
  expect_equal(exn(AM92Lt, x = 65, n = 10), 9.268781578248, tolerance = 1e-6)
  expect_equal(exn(usaMale07Lt, x = 40, n = 25), 23.416864695062, tolerance = 1e-6)
})

test_that("exn (Kx) is consistent with a naive per-year sum of pxt", {
  # Independent, unoptimized reimplementation of the Kx branch (a plain
  # loop calling pxt one year at a time), used only as an oracle to check
  # the vectorised implementation -- not the implementation under test.
  naiveExnKx <- function(object, x, n) {
    probs <- numeric(n)
    for (i in 1:n) probs[i] <- pxt(object, x, i)
    sum(probs)
  }

  cases <- list(
    list(tb = soa08Act, x = 30, n = 15),
    list(tb = soa08Act, x = 100, n = 5),
    list(tb = ips55M, x = 45, n = 30),
    list(tb = AF92Lt, x = 60, n = 20),
    list(tb = usaMale07Lt, x = 55, n = 18)
  )
  for (cs in cases) {
    expect_equal(
      exn(cs$tb, x = cs$x, n = cs$n, type = "Kx"),
      naiveExnKx(cs$tb, cs$x, cs$n),
      tolerance = 1e-10
    )
  }

  # n = 1 edge case
  expect_equal(exn(soa08Act, x = 50, n = 1, type = "Kx"), pxt(soa08Act, 50, 1))

  # near the very end of the table, the expected residual curtate lifetime
  # should be a small non-negative number
  omega <- getOmega(soa08Act)
  val <- exn(soa08Act, x = omega - 1, n = 1, type = "Kx")
  expect_true(val >= 0 && val < 1)
})

test_that("exn type='Tx'/'complete' branch (untouched code path) still agrees with Lxt", {
  expect_equal(exn(soa08Act, x = 0, type = "complete"),
               Lxt(soa08Act, x = 0, t = getOmega(soa08Act) + 1) / soa08Act@lx[1],
               tolerance = 1e-10)
})

## ---------------------------------------------------------------------
## probs2lifetable(): vectorised cumprod() vs the previous explicit loop.
## ---------------------------------------------------------------------

test_that("probs2lifetable satisfies its defining recursion on real mortality data", {
  # qx derived from a genuine, bundled Italian mortality table (IPS55M),
  # used the way probs2lifetable is meant to be used: build a lifetable
  # back from raw one-year probabilities (see the "clean lx series"
  # discussion in the package vignette).
  qxReal <- 1 - lxIPS55M[-1] / lxIPS55M[-length(lxIPS55M)]
  radix <- 10000
  lt <- probs2lifetable(probs = qxReal, radix = radix, type = "qx", name = "ips55 rebuilt")

  # defining recursion: lx[1] = radix, lx[i+1] = lx[i] * (1 - qx[i])
  expect_equal(lt@lx[1], radix)
  expected <- radix * cumprod(1 - qxReal)
  expect_equal(lt@lx[-1][seq_along(expected)], expected, tolerance = 1e-8)
  expect_true(all(diff(lt@lx) <= 0))          # lx must be non-increasing
  expect_equal(length(lt@lx), length(lt@x))
  expect_equal(lt@x, seq(0, length(lt@x) - 1))

  # same check starting from px instead of qx
  pxReal <- lxIPS55M[-1] / lxIPS55M[-length(lxIPS55M)]
  ltPx <- probs2lifetable(probs = pxReal, radix = radix, type = "px", name = "ips55 rebuilt px")
  expect_equal(ltPx@lx, lt@lx, tolerance = 1e-8)
})

test_that("probs2lifetable handles edge-case inputs safely", {
  # already-terminated vectors: no extra row gets appended, so (as in the
  # pre-existing implementation) the trailing 0/1 marker itself is not
  # consumed by the recursion -- only probs[1:(n-1)] feed lx[2:n].
  ltPx <- probs2lifetable(probs = c(0.9, 0.8, 0.5, 0), type = "px", radix = 1000)
  expect_equal(length(ltPx@lx), 4)
  expect_equal(ltPx@lx[4], 1000 * 0.9 * 0.8 * 0.5)

  ltQx <- probs2lifetable(probs = c(0.01, 0.05, 0.2, 1), type = "qx", radix = 1000)
  expect_equal(length(ltQx@lx), 4)
  expect_equal(ltQx@lx[4], 1000 * (1 - 0.01) * (1 - 0.05) * (1 - 0.2))

  # single-probability input (degenerate but should not error)
  ltOne <- probs2lifetable(probs = c(0.5), type = "px", radix = 1000)
  expect_equal(ltOne@lx, c(1000, 500))

  # invalid probabilities are rejected
  expect_error(probs2lifetable(probs = c(0.5, 1.5), type = "px"))
  expect_error(probs2lifetable(probs = c(0.5, -0.1), type = "qx"))
  expect_error(probs2lifetable(probs = c(0.5, 0.4), type = "invalid"))
})

test_that("a lifetable built by probs2lifetable reproduces the input probabilities via pxt/qxt", {
  qxReal <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5)
  lt <- probs2lifetable(probs = qxReal, radix = 100000, type = "qx", name = "check")
  for (i in seq_along(qxReal)) {
    expect_equal(qxt(lt, x = i - 1, t = 1), qxReal[i], tolerance = 1e-8)
  }
})

## ---------------------------------------------------------------------
## exyzt(): only smoke-tested (expect_no_error) elsewhere in the suite
## (tests/testthat/test-legacy-demography.R). Added here as numeric
## regression values on a real table, since the vectorised matrix-based
## implementation had no value-level test yet.
## ---------------------------------------------------------------------

test_that("exyzt matches known reference values on soa08Act", {
  expect_equal(
    exyzt(list(soa08Act, soa08Act), x = c(55, 50), t = 10, status = "joint"),
    8.9920791965, tolerance = 1e-6
  )
  expect_equal(
    exyzt(list(soa08Act, soa08Act), x = c(55, 50), t = 10, status = "last"),
    9.9642714038, tolerance = 1e-6
  )
  expect_equal(
    exyzt(list(soa08Act, soa08Act), x = c(60, 58), status = "joint"),
    13.8762657924, tolerance = 1e-6
  )
})

test_that("exyzt (joint) is consistent with an independent per-year sum of pxyzt", {
  naiveExyztJoint <- function(tablesList, x, term) {
    out <- 0
    for (j in 1:term) out <- out + pxyzt(tablesList = tablesList, x = x, t = j, status = "joint")
    out
  }
  term <- 12
  expect_equal(
    exyzt(list(soa08Act, soa08Act), x = c(62, 59), t = term, status = "joint"),
    naiveExyztJoint(list(soa08Act, soa08Act), x = c(62, 59), term = term),
    tolerance = 1e-10
  )
})
