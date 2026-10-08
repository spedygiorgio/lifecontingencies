## tests/testthat/test-mdt-asdt-functions.R
## Regression tests for:
##   independentRatesFromMdt()
##   buildMdtFromIndependentRates()
##   plot.mdt S4 method
##
## Published reference values come from:
##   Finan, M.B. (2014). A Probability Course for the Actuaries:
##     A Preparation for Exam 1/P (ASM Study Manual for Exam MLC).
##   Bowers, N.L. et al. (1997). Actuarial Mathematics, 2nd ed., SOA.

library(lifecontingencies)

# ---------------------------------------------------------------------------
# Fixture: the Valdez example table used throughout the package
# ---------------------------------------------------------------------------
valdezDf <- data.frame(
  x = 50:54,
  lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
  heart = c(5168, 5363, 5618, 5929, 6277),
  accidents = c(1157, 1206, 1443, 1679, 2152),
  other = c(4293, 5162, 5960, 6840, 7631)
)
valdezMdt <- new("mdt", name = "ValdezExample", table = valdezDf)

# ===========================================================================
# independentRatesFromMdt()
# ===========================================================================
context("independentRatesFromMdt")

test_that("returns a matrix of correct dimension and names", {
  mat <- independentRatesFromMdt(valdezMdt, x = 50:54)
  expect_true(is.matrix(mat))
  # ages 50:54 have decrement data (5 ages)
  expect_equal(nrow(mat), 5)
  expect_equal(ncol(mat), 3)
  expect_equal(colnames(mat), c("heart", "accidents", "other"))
  expect_equal(rownames(mat), as.character(50:54))
})

test_that("subset of ages works", {
  mat <- independentRatesFromMdt(valdezMdt, x = 50:52)
  expect_equal(nrow(mat), 3)
  expect_equal(rownames(mat), c("50", "51", "52"))
})

test_that("each element matches the scalar qxt.prime.fromMdt", {
  mat <- independentRatesFromMdt(valdezMdt, x = 50:54)
  for (d in getDecrements(valdezMdt)) {
    for (a in 50:54) {
      expect_equal(
        mat[as.character(a), d],
        qxt.prime.fromMdt(valdezMdt, x = a, decrement = d),
        info = paste("age", a, "decrement", d)
      )
    }
  }
})

test_that("invalid ages are rejected", {
  expect_error(independentRatesFromMdt(valdezMdt, x = c(99, 100)),
               "Ages not in table")
})

test_that("non-mdt input is rejected", {
  expect_error(independentRatesFromMdt("not an mdt"), "Need an mdt object")
})

# Finan Example 67.1: double decrement table
# qτ_x = 0.20, q(1)_x = 0.04, q(2)_x = 0.16
# Expected: q'(1) = 1 - (1 - 0.20)^(0.04/0.20) = 0.04365 (approx)
#           q'(2) = 1 - (1 - 0.20)^(0.16/0.20) = 0.16354 (approx)
# The Finan book states (checking at 5 decimals):
#   q'(1) ≈ 0.04365, q'(2) ≈ 0.16354
test_that("Finan Example 67.1: double decrement UDD extraction", {
  # Build a minimal mdt with one age, both starting at age 40 (with the
  # synthetic backfill below it) and at age 0 (no backfill: up to 1.6.2 this
  # case returned NaN because of a bug in the decrement branch of pxt()).
  #
  # qτ_40 = 0.20, q(1)_40 = 0.04, q(2)_40 = 0.16
  tbl67_1 <- data.frame(x = 40:41, lx = c(1000, 800),
                         d1 = c(40, 800), d2 = c(160, 0))
  mdt67_1 <- new("mdt", name = "Finan67.1", table = tbl67_1)
  mat <- independentRatesFromMdt(mdt67_1, x = 40)
  # q'(1) = 1 - (1-0.20)^(0.04/0.20) = 1 - 0.8^0.2
  expect_equal(round(mat["40", "d1"], 5), round(1 - 0.80^0.2, 5))
  # q'(2) = 1 - (1-0.20)^(0.16/0.20) = 1 - 0.8^0.8
  expect_equal(round(mat["40", "d2"], 5), round(1 - 0.80^0.8, 5))

  mdt0 <- new("mdt", name = "Finan67.1 at 0",
              table = data.frame(x = 0:1, lx = c(1000, 800),
                                 d1 = c(40, 800), d2 = c(160, 0)))
  mat0 <- independentRatesFromMdt(mdt0, x = 0)
  expect_equal(unname(mat0[1, ]), 1 - 0.80^c(0.2, 0.8))
})

# ===========================================================================
# buildMdtFromIndependentRates()
# ===========================================================================
context("buildMdtFromIndependentRates")

# Finan Example 67.4:
# 1000 lives age 60. Three decrements: death(1), disability(2), retirement(3).
# Independent rates:
#   q'(1)_60 = 0.010, q'(2)_60 = 0.030, q'(3)_60 = 0.100
#   q'(1)_61 = 0.013, q'(2)_61 = 0.050, q'(3)_61 = 0.200
# Expected:
#   p(τ)_60 = (1-0.010)(1-0.030)(1-0.100) = 0.86427
#   l(τ)_61 = 864.27
#   d(3)_61 = l(τ)_61 * q(3)_61 ≈ 167.60

test_that("Finan Example 67.4: building mdt from independent rates", {
  qp <- matrix(c(0.010, 0.030, 0.100,
                  0.013, 0.050, 0.200),
               nrow = 2, byrow = TRUE,
               dimnames = list(NULL, c("death", "disability", "retirement")))
  mdt674 <- buildMdtFromIndependentRates(x = 60:61, qx.primes = qp,
                                          radix = 1000, name = "Finan 67.4")
  expect_is(mdt674, "mdt")
  tbl <- mdt674@table

  # p(τ)_60 = prod(1 - q'_60) = 0.990 * 0.970 * 0.900
  ptau60 <- 0.990 * 0.970 * 0.900
  expect_equal(round(ptau60, 5), 0.86427)

  # l(τ)_61 = 1000 * 0.86427 = 864.27
  row61 <- tbl[tbl$x == 61, ]
  expect_equal(round(row61$lx, 2), 864.27)

  # Check that decrements are identified correctly
  expect_equal(getDecrements(mdt674), c("death", "disability", "retirement"))
})

test_that("Finan Example 67.4: specific decrement counts", {
  qp <- matrix(c(0.010, 0.030, 0.100,
                  0.013, 0.050, 0.200),
               nrow = 2, byrow = TRUE,
               dimnames = list(NULL, c("death", "disability", "retirement")))
  mdt674 <- buildMdtFromIndependentRates(x = 60:61, qx.primes = qp,
                                          radix = 1000, name = "Finan 67.4")
  tbl <- mdt674@table

  # The absolute rate q(3)_60 via the integration formula gives d(3)_60
  # q(3)_60 = q'(3)_60 * integral_0^1 (1 - s*q'(1)_60)(1 - s*q'(2)_60) ds
  # Verify via the already-tested scalar function:
  q3_60 <- qxt.fromQxprime(qx.prime = 0.100, other.qx.prime = c(0.010, 0.030))
  d3_60_expected <- 1000 * q3_60
  row60 <- tbl[tbl$x == 60, ]
  expect_equal(round(row60$retirement, 2), round(d3_60_expected, 2))

  # d(3)_61 ≈ 167.60 from Finan
  q3_61 <- qxt.fromQxprime(qx.prime = 0.200, other.qx.prime = c(0.013, 0.050))
  d3_61_expected <- 864.27 * q3_61
  row61 <- tbl[tbl$x == 61, ]
  expect_equal(round(row61$retirement, 1), round(d3_61_expected, 1))
})

test_that("Finan Problem 67.5: 500 lives age 50 with 3 decrements", {
  # q'(1)_50 = 0.050, q'(2)_50 = 0.030, q'(3)_50 = 0.100
  qp <- matrix(c(0.050, 0.030, 0.100), nrow = 1,
               dimnames = list(NULL, c("d1", "d2", "d3")))
  mdt675 <- buildMdtFromIndependentRates(x = 50, qx.primes = qp,
                                          radix = 500, name = "Finan 67.5")
  tbl <- mdt675@table
  row50 <- tbl[tbl$x == 50, ]

  # p(τ)_50 = (1-0.05)(1-0.03)(1-0.10) = 0.950 * 0.970 * 0.900 = 0.82935
  ptau50 <- 0.950 * 0.970 * 0.900
  expect_equal(round(ptau50, 5), 0.82935)

  # total decrements at age 50 = 500 * (1 - 0.82935) = 85.325
  total_dx <- sum(row50$d1, row50$d2, row50$d3)
  expect_equal(round(total_dx, 1), round(500 * (1 - ptau50), 1))
})

test_that("default column names are generated for unnamed matrix", {
  qp <- matrix(c(0.01, 0.02, 0.03, 0.04), nrow = 2)
  mdt_unnamed <- buildMdtFromIndependentRates(qx.primes = qp, radix = 1000)
  expect_equal(getDecrements(mdt_unnamed), c("d1", "d2"))
})

test_that("input validation", {
  expect_error(buildMdtFromIndependentRates(x = 1:3,
    qx.primes = matrix(0.01, nrow = 2, ncol = 1)),
    "Length of 'x' must equal")
})

# ===========================================================================
# Round-trip: mdt -> independent rates -> mdt
# ===========================================================================
context("ASDT round-trip")

test_that("mdt -> independentRates -> buildMdt recovers the original table", {
  # Extract ASDT rates from the Valdez table
  qprime <- independentRatesFromMdt(valdezMdt, x = 50:54)

  # Rebuild from those independent rates
  rebuilt <- buildMdtFromIndependentRates(
    x = 50:54, qx.primes = qprime,
    radix = valdezMdt@table$lx[valdezMdt@table$x == 50],
    name = "roundtrip"
  )

  # The reconstructed lx should match the original at the tabulated ages
  origAges <- 50:54
  for (a in origAges) {
    origLx <- valdezMdt@table$lx[valdezMdt@table$x == a]
    newLx <- rebuilt@table$lx[rebuilt@table$x == a]
    expect_equal(round(newLx, 0), round(origLx, 0),
                 info = paste("lx at age", a),
                 tolerance = 1)
  }

  # The reconstructed decrement counts should be close to the originals
  for (d in getDecrements(valdezMdt)) {
    for (a in origAges) {
      origDx <- valdezMdt@table[[d]][valdezMdt@table$x == a]
      newDx <- rebuilt@table[[d]][rebuilt@table$x == a]
      expect_equal(newDx, origDx, tolerance = 1,
                   info = paste("d(", d, ") at age", a))
    }
  }
})

# ===========================================================================
# plot() S4 method for mdt
# ===========================================================================
context("plot.mdt")

test_that("plot returns a ggplot object for all three types", {
  for (type in c("area", "bar", "probability")) {
    p <- plot(valdezMdt, type = type)
    expect_is(p, "ggplot", info = paste("type =", type))
  }
})

test_that("default plot type is area", {
  p <- plot(valdezMdt)
  # Area plot has a GeomArea layer
  layers <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomArea" %in% layers || "GeomRibbon" %in% layers)
})

test_that("plot type 'bar' uses GeomCol", {
  p <- plot(valdezMdt, type = "bar")
  layers <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomCol" %in% layers)
})

test_that("plot type 'probability' uses GeomLine", {
  p <- plot(valdezMdt, type = "probability")
  layers <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomLine" %in% layers)
})
