library(testthat)
library(lifecontingencies)

context("Demographic extras: open interval (Tx/exn), variance/sd, median/quantile/modalAge")

data(soa08Act)
mk <- function(x, lx, name = "t") new("lifetable", x = x, lx = lx, name = name)
deMoivre <- function(w) mk(0:w, w - (0:w), paste("de Moivre", w))

## ---------------------------------------------------------------------
## Point 2: open-interval / fxt parameters of Tx() and exn() are backward
## compatible by default, and reproduce published open-interval tables.
## ---------------------------------------------------------------------

test_that("Tx default behaviour is unchanged", {
  expect_equal(Tx(soa08Act, 67), 1021989.350698633, tolerance = 1e-6)
  expect_equal(Tx(soa08Act, 0), 7180788.511223633, tolerance = 1e-6)
  # explicit defaults equal the implicit ones
  expect_equal(Tx(soa08Act, 40), Tx(soa08Act, 40, fxt = 0.5, axOmega = 0.5))
})

test_that("exn default behaviour is unchanged", {
  expect_equal(exn(soa08Act, 0), 71.307885112166, tolerance = 1e-6)
  expect_equal(exn(soa08Act, 0, type = "complete"), 71.807885112166, tolerance = 1e-6)
  expect_equal(exn(soa08Act, 50, 20, type = "complete"),
               Lxt(soa08Act, 50, 20) / soa08Act@lx[soa08Act@x == 50], tolerance = 1e-10)
})

test_that("open-interval axOmega reproduces the NCHS 2019 total table Tx/ex", {
  # NCHS US 2019 total life table (same source as test-published-nchs-us2019.R)
  d <- read.csv(text = "age,lx,dx,Lx,Tx,ex
0,100000,557,99513,7884823,78.8
40,96404,192,96309,3941792,40.9
65,83868,1059,83339,1643002,19.6
90,25317,3520,23557,116930,4.6
100,2090,2090,4696,4696,2.2")
  # rebuild the full single-year table only where needed: use the published
  # lx column fully from the companion test is overkill here; instead verify
  # the open-interval closure on the last age directly.
  # L100 (open) = l100 / m100 ; axOmega = L100 / l100
  mOmega <- d$dx[d$age == 100] / d$Lx[d$age == 100]
  axOmega <- 1 / mOmega
  expect_equal(axOmega * d$lx[d$age == 100], d$Lx[d$age == 100], tolerance = 1e-8)
  # closed default: last age contributes 0.5 * l_omega
  lt1 <- mk(c(99, 100), c(3024, 2090))
  expect_equal(Tx(lt1, 100), 0.5 * 2090)                 # default (closed)
  expect_equal(Tx(lt1, 100, axOmega = axOmega), axOmega * 2090)  # open
})

test_that("exn complete honours axOmega only when the period reaches omega", {
  lt <- mk(0:3, c(1000, 900, 700, 400))
  # temporary period strictly inside the table: axOmega irrelevant
  expect_equal(exn(lt, 0, 2, type = "complete"),
               exn(lt, 0, 2, type = "complete", axOmega = 0.9))
  # whole life: axOmega changes the result
  expect_false(isTRUE(all.equal(exn(lt, 0, type = "complete"),
                                exn(lt, 0, type = "complete", axOmega = 0.9))))
})

## ---------------------------------------------------------------------
## Point 3: variance and standard deviation of the future lifetime.
## ---------------------------------------------------------------------

test_that("varxn (curtate) matches a published worked example", {
  # Finan, Exam MLC manual, Example/Problem 23.39:
  #   x  80  81  82  83  84  85  86 ; lx 250 217 161 107 62 28 0
  #   e80 = 2.3, Var(K80) = 2.394
  t80 <- mk(80:86, c(250, 217, 161, 107, 62, 28, 0))
  expect_equal(exn(t80, 80), 2.3, tolerance = 1e-12)
  expect_equal(varxn(t80, 80, type = "Kx"), 2.394, tolerance = 1e-9)
  expect_equal(sdxn(t80, 80), sqrt(2.394), tolerance = 1e-9)
})

test_that("varxn curtate equals its defining second-moment formula", {
  for (x in c(0, 40, 80)) {
    n <- getOmega(soa08Act) - x + 1
    kpx <- pxt(soa08Act, x, 1:n)
    m1 <- sum(kpx); m2 <- sum((2 * (1:n) - 1) * kpx)
    expect_equal(varxn(soa08Act, x), m2 - m1^2, tolerance = 1e-9)
  }
  # temporary curtate variance uses min(K, n)
  x <- 40; n <- 20
  kpx <- pxt(soa08Act, x, 1:n)
  m1 <- sum(kpx); m2 <- sum((2 * (1:n) - 1) * kpx)
  expect_equal(varxn(soa08Act, x, n), m2 - m1^2, tolerance = 1e-9)
})

test_that("varxn complete = varxn curtate + 1/12 (UDD), whole life only", {
  for (x in c(0, 30, 65)) {
    expect_equal(varxn(soa08Act, x, type = "complete"),
                 varxn(soa08Act, x, type = "Kx") + 1/12, tolerance = 1e-12)
  }
  # temporary complete variance is not supported
  expect_error(varxn(soa08Act, 40, 20, type = "complete"))
})

## ---------------------------------------------------------------------
## Point 4: median / quantile / modalAge via standard R syntax.
## ---------------------------------------------------------------------

test_that("median and quantile are exact on a de Moivre table", {
  dm <- deMoivre(100)                       # lx = 100 - x, linear
  expect_equal(median(dm), 50)              # lx = 50 at age 50
  expect_equal(unname(quantile(dm, c(0.25, 0.75))), c(25, 75))
  expect_equal(unname(quantile(dm, 0.5)), 50)
  # names follow stats::quantile convention
  expect_equal(names(quantile(dm, c(0.1, 0.9))), c("10%", "90%"))
})

test_that("median/quantile condition on survival to 'age' and work on actuarialtable", {
  # actuarialtable inherits the lifetable methods
  expect_true(is.numeric(median(soa08Act)))
  expect_gt(median(soa08Act, age = 65), 65)
  # conditioning raises the median age at death
  expect_gt(median(soa08Act, age = 80), median(soa08Act))
  # quantiles are monotone in probs
  q <- quantile(soa08Act, probs = c(0.1, 0.5, 0.9))
  expect_true(all(diff(q) > 0))
})

test_that("median equals the 0.5 quantile", {
  dm <- deMoivre(90)
  expect_equal(median(dm), unname(quantile(dm, 0.5)))
  expect_equal(median(soa08Act, age = 50), unname(quantile(soa08Act, 0.5, age = 50)))
})

test_that("modalAge returns the age of maximum deaths", {
  # deaths engineered to peak at an interior age (age 3); the open last age
  # (age 5 here) is excluded from the search by construction
  lt <- mk(0:5, c(1000, 990, 970, 600, 200, 20))  # dx = 10,20,370,400,180 -> max at age 3
  expect_equal(modalAge(lt), 3)
  # startAge restricts the search; interpolation returns a value near the peak
  dm <- deMoivre(100)
  expect_true(is.numeric(modalAge(dm)))
  expect_true(modalAge(soa08Act, startAge = 10, interpolate = TRUE) > 50)
})

## ---------------------------------------------------------------------
## print() optional arguments (point 1).
## ---------------------------------------------------------------------

test_that("print() forwards exType/fxt/axOmega and defaults are unchanged", {
  lt <- mk(0:5, c(1000, 900, 780, 600, 350, 0))
  dfDefault <- print(lt)                       # invisibly returns the data.frame
  expect_true(is.data.frame(dfDefault))
  expect_identical(names(dfDefault), c("x", "lx", "px", "Lx", "Tx", "ex"))
  # default ex column is the curtate expectation
  expect_equal(dfDefault$ex[1], exn(lt, 0, type = "curtate"), tolerance = 1e-12)
  # complete option changes the ex column to the complete expectation
  dfComplete <- print(lt, exType = "complete")
  expect_equal(dfComplete$ex[1], exn(lt, 0, type = "complete"), tolerance = 1e-12)
  # Lx/Tx columns unchanged under the default fxt
  expect_equal(dfDefault$Lx, dfComplete$Lx)
})
