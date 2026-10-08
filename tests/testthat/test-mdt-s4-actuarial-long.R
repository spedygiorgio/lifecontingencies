## Tests for the 2026-10 mdt work package:
##   * dxt()/pxt()/qxt() as S4 generics (P3.1) and the fix of the
##     decrement-specific branch of pxt() on mdt objects;
##   * Axn.mdt() with several decrements / cause-dependent benefits and
##     axn.mdt() (P2.3), checked against worked examples printed in
##     Finan, M.B. (2014), "A Reading of the Theory of Life Contingency
##     Models", Sections 68-69;
##   * mdtToLong() (P4.1), checked against the Aalen-Johansen estimator of
##     the survival package.

library(lifecontingencies)

valdezMdt <- new("mdt", name = "ValdezExample", table = data.frame(
  x = 50:54,
  lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
  heart = c(5168, 5363, 5618, 5929, 6277),
  accidents = c(1157, 1206, 1443, 1679, 2152),
  other = c(4293, 5162, 5960, 6840, 7631)))

# a table starting at age 0 with no zero cell in its first row: this is the
# configuration that triggered the pxt() bug
mdt0 <- new("mdt", name = "noZeros", table = data.frame(
  x = 0:2, lx = c(1000, 800, 500), d1 = c(50, 100, 200), d2 = c(150, 200, 300)))

context("S4 generics dxt/pxt/qxt")

test_that("dxt, pxt and qxt are S4 generics with lifetable and mdt methods", {
  for (f in c("dxt", "pxt", "qxt")) {
    expect_true(isGeneric(f), info = f)
    expect_true(existsMethod(f, "lifetable"), info = f)
    expect_true(existsMethod(f, "mdt"), info = f)
  }
  data(soaLt)
  soa <- with(soaLt, new("actuarialtable", interest = .06, x = x, lx = Ix))
  # actuarialtable inherits the lifetable method
  expect_equal(pxt(soa, 50, 10), soaLt$Ix[soaLt$x == 60] / soaLt$Ix[soaLt$x == 50])
})

test_that("unsupported objects keep the historical error message", {
  for (f in list(dxt, pxt, qxt))
    expect_error(f("not a table", 1, 1), "Only lifetable, actuarialtable or mdt")
})

test_that("t defaults to 1 as documented", {
  data(soaLt)
  soa <- with(soaLt, new("actuarialtable", interest = .06, x = x, lx = Ix))
  expect_equal(pxt(soa, 50), pxt(soa, 50, 1))
  expect_equal(qxt(soa, 50), qxt(soa, 50, 1))
  expect_equal(qxt(valdezMdt, 51), qxt(valdezMdt, 51, 1))
})

context("decrement-specific probabilities on mdt")

test_that("decrement-specific qxt is correct when the first row has no zeros", {
  # previously NaN at age 0 and -2 at age 1
  expect_equal(qxt(mdt0, 0:2, 1, decrement = "d1"), c(50/1000, 100/800, 200/500))
  expect_equal(qxt(mdt0, 0:2, 1, decrement = "d2"), c(150/1000, 200/800, 300/500))
  expect_equal(qxt(mdt0, 0, 1:3, decrement = "d1"), cumsum(c(50, 100, 200)) / 1000)
  expect_equal(pxt(mdt0, 1, 1, decrement = 2), 1 - 200/800)
})

test_that("cause-specific probabilities add up to the total one", {
  for (a in 50:54) for (k in 1:(55 - a)) {
    byCause <- sum(sapply(getDecrements(valdezMdt), function(d)
      qxt(valdezMdt, a, k, decrement = d)))
    expect_equal(byCause, qxt(valdezMdt, a, k), info = paste(a, k))
  }
})

test_that("several decrements can be passed at once", {
  expect_equal(qxt(mdt0, 0, 2, decrement = c("d1", "d2")), qxt(mdt0, 0, 2))
  expect_equal(dxt(valdezMdt, 50, 2, decrement = c("heart", "other")),
               5168 + 5363 + 4293 + 5162)
  expect_error(qxt(mdt0, 0, 1, decrement = c("d1", "zz")), "Not recognized")
})

test_that("fractional durations interpolate linearly (UDD) within the year", {
  expect_equal(qxt(valdezMdt, 50, 1.5, decrement = "heart"),
               (5168 + 0.5 * 5363) / 4832555)
  expect_equal(dxt(valdezMdt, 50, 1.5, decrement = "heart"), 5168 + 0.5 * 5363)
})

test_that("Valdez reference values are unchanged by the refactor", {
  expect_equal(dxt(valdezMdt, x = 51, t = 2, decrement = "other"), 11122)
  expect_equal(round(pxt(valdezMdt, x = 50, t = 3), 5), 0.99268)
  expect_equal(round(qxt(valdezMdt, x = 50, t = 3, decrement = "heart"), 5), 0.00334)
})

context("Axn.mdt / axn.mdt (Finan 2014, Sections 68-69)")

finan691 <- new("mdt", name = "Finan 69.1", table = data.frame(
  x = 16:18, lx = c(20000, 17600, 14520),
  da = c(1300, 1870, 2380), doc = c(1100, 1210, 1331)))

test_that("Finan Example 69.1: APV 3000, annuity 2.4, premium 1250", {
  # Finan's printed solution uses the 'doc' column (1100, 1210, 1331)
  A <- Axn.mdt(finan691, x = 16, n = 3, i = 0.10, decrement = "doc")
  a <- axn.mdt(finan691, x = 16, n = 3, i = 0.10)
  expect_equal(20000 * A, 3000)
  expect_equal(a, 2.4)
  expect_equal(20000 * A / a, 1250)
})

test_that("Finan Example 68.1: cause-dependent benefits (1 and 2), i = 50%", {
  m <- new("mdt", table = data.frame(x = 50:51, lx = c(1200, 800),
                                     d1 = c(100, 200), d2 = c(300, 300)))
  apv <- Axn.mdt(m, x = 50, n = 2, i = 0.5, decrement = c("d1", "d2"),
                 benefits = c(1, 2))
  expect_equal(round(apv, 4), 0.6852)
  # equals the sum of the single-cause APVs
  expect_equal(apv, Axn.mdt(m, 50, 2, .5, "d1") + 2 * Axn.mdt(m, 50, 2, .5, "d2"))
})

test_that("Finan Example 69.2: benefit reserve 2V = 11.091", {
  m <- new("mdt", table = data.frame(x = 41:43, lx = c(800, 776, 752),
                                     d1 = 8, d2 = 16))
  i <- 1 / 0.95 - 1
  V2 <- Axn.mdt(m, 42, 2, i, c("d1", "d2"), benefits = c(2000, 1000)) -
    34 * axn.mdt(m, 42, 2, i)
  expect_equal(round(V2, 3), 11.091)
})

test_that("Finan Example 69.3: E[1L | K(55) > 1] = 16.72", {
  q1 <- c(.002, .005, .008); q2 <- c(.02, .04, .06)
  l <- 100000 * cumprod(c(1, 1 - (q1 + q2)[1:2]))
  m <- new("mdt", table = data.frame(x = 55:57, lx = l, d1 = l * q1, d2 = l * q2))
  expect_equal(round(Axn.mdt(m, 56, 2, .06, "d1"), 10), 0.0115165539)
  expect_equal(round(Axn.mdt(m, 56, 2, .06, "d2"), 10), 0.0887326451)
  expect_equal(round(Axn.mdt(m, 56, 2, .06, c(1, 2), c(2000, 1000)), 2), 111.77)
  expect_equal(round(50 * axn.mdt(m, 56, 2, .06), 2), 95.05)
  expect_equal(round(Axn.mdt(m, 56, 2, .06, c(1, 2), c(2000, 1000)) -
                       50 * axn.mdt(m, 56, 2, .06), 2), 16.72)
})

test_that("missing decrement means cover on the total decrement", {
  expect_equal(Axn.mdt(finan691, 16, 3, .1),
               Axn.mdt(finan691, 16, 3, .1, c("da", "doc")))
})

test_that("default n runs to the end of the table (omega + 1 - x - m)", {
  omega <- getOmega(finan691)
  expect_equal(Axn.mdt(finan691, 16, i = .1, decrement = "da"),
               Axn.mdt(finan691, 16, omega + 1 - 16, .1, "da"))
  expect_equal(axn.mdt(finan691, 16, i = .1),
               axn.mdt(finan691, 16, omega + 1 - 16, .1))
})

test_that("axn.mdt: arrears, deferment, m-thly payments and vectorisation", {
  v <- 1 / 1.1
  expect_equal(axn.mdt(finan691, 16, 2, .1, payment = "arrears"),
               v * 17600 / 20000 + v^2 * 14520 / 20000)
  expect_equal(axn.mdt(finan691, 16, 1, .1, m = 1),
               v * 17600 / 20000)
  # monthly due annuity: payments at 0, 1/12, ..., survival linear in lx
  tt <- (0:23) / 12
  expect_equal(axn.mdt(finan691, 16, 2, .1, k = 12),
               sum(1.1^-tt * pxt(finan691, 16, tt)) / 12)
  expect_equal(axn.mdt(finan691, c(16, 17), 1, .1), c(1, 1))
  expect_equal(Axn.mdt(finan691, c(16, 17), 1, .1, "da"),
               c(1300 / 20000, 1870 / 17600) * v)
})

test_that("input validation", {
  expect_error(Axn.mdt("x", 16, 3, .1), "Needed Mdt")
  expect_error(axn.mdt(finan691, 16, 3), "Missing interest")
  expect_error(Axn.mdt(finan691, 16, 3, .1, "da", benefits = c(1, 2, 3)),
               "More benefits")
})

context("mdtToLong")

test_that("long format preserves all lives and has the right structure", {
  long <- mdtToLong(valdezMdt, x = 50, t = 5)
  expect_equal(names(long), c("time", "age", "status", "count"))
  expect_equal(levels(long$status), c("censored", getDecrements(valdezMdt)))
  expect_equal(sum(long$count), 4832555)
  expect_equal(long$count[long$status == "censored"], 4766677)
  expect_equal(mdtToLong(valdezMdt, 50, 5, exitTime = "mid")$time[1], 0.5)
  expect_error(mdtToLong(valdezMdt, x = 50.5), "single age")
})

test_that("Aalen-Johansen cumulative incidence equals qxt by cause", {
  skip_if_not_installed("survival")
  long <- mdtToLong(valdezMdt, x = 50, t = 5)
  fit <- survival::survfit(survival::Surv(time, status) ~ 1,
                           data = long, weights = count)
  ps <- summary(fit, times = 1:5)$pstate
  for (j in seq_along(getDecrements(valdezMdt))) {
    d <- getDecrements(valdezMdt)[j]
    expect_equal(ps[, j + 1], qxt(valdezMdt, 50, 1:5, decrement = d),
                 tolerance = 1e-12, info = d)
  }
  expect_equal(ps[, 1], pxt(valdezMdt, 50, 1:5), tolerance = 1e-12)
})
