test_that("optimized axn matches legacy scalar calculations", {
  data(soaLt)
  tab <- with(soaLt, new("actuarialtable", interest = 0.06,
                           x = x, lx = Ix, name = "SOA2008"))

  x <- c(35, 50, 65, 75)
  n <- c(10, 15, 20, 8)
  m <- c(0, 2, 5, 1)

  for (payment in c("advance", "immediate")) {
    for (k in c(1, 2, 4, 12)) {
      expected <- vapply(seq_along(x), function(j) {
        axnold(tab, x = x[j], n = n[j], m = m[j], k = k,
               payment = payment)
      }, numeric(1))
      actual <- axn(tab, x = x, n = n, m = m, k = k,
                    payment = payment)
      expect_equal(actual, expected, tolerance = 1e-10)
    }
  }
})

test_that("optimized Axn matches legacy scalar calculations", {
  data(soaLt)
  tab <- with(soaLt, new("actuarialtable", interest = 0.06,
                           x = x, lx = Ix, name = "SOA2008"))

  x <- c(35, 50, 65, 75)
  n <- c(10, 15, 20, 8)
  m <- c(0, 2, 5, 1)

  for (k in c(1, 2, 4, 12)) {
    expected <- vapply(seq_along(x), function(j) {
      Axnold(tab, x = x[j], n = n[j], m = m[j], k = k)
    }, numeric(1))
    actual <- Axn(tab, x = x, n = n, m = m, k = k)
    expect_equal(actual, expected, tolerance = 1e-10)
  }
})

test_that("optimized AExn matches Axn + Exn", {
  data(soaLt)
  tab <- with(soaLt, new("actuarialtable", interest = 0.06,
                           x = x, lx = Ix, name = "SOA2008"))

  x <- c(35, 50, 65, 75)
  n <- c(10, 15, 20, 8)

  for (k in c(1, 2, 4, 12)) {
    expected <- vapply(seq_along(x), function(j) {
      Axnold(tab, x = x[j], n = n[j], k = k) +
        Exn(tab, x = x[j], n = n[j])
    }, numeric(1))
    actual <- AExn(tab, x = x, n = n, k = k)
    expect_equal(actual, expected, tolerance = 1e-10)
  }
})

test_that("optimized APVs support power and fractional assumptions", {
  data(soaLt)
  tab <- with(soaLt, new("actuarialtable", interest = 0.06,
                           x = x, lx = Ix, name = "SOA2008"))

  for (fractional in c("linear", "constant force", "hyperbolic")) {
    for (power in c(1, 2)) {
      times_ax <- 2 + seq(from = 1 / 4, to = 12, by = 1 / 4)
      p_ax <- pxt(tab, x = rep(60, length(times_ax)), t = times_ax,
                  fractional = fractional)
      expected_ax <- sum((1 / 4)^power *
        (1 + tab@interest)^(-times_ax * power) * p_ax)
      actual_ax <- axn(tab, x = 60, n = 12, m = 2, k = 4,
                       power = power, fractional = fractional)
      expect_equal(actual_ax, expected_ax, tolerance = 1e-10)

      times_A <- 2 + seq(from = 0, to = 12 - 1 / 4, by = 1 / 4)
      p_A <- pxt(tab, x = rep(60, length(times_A)), t = times_A,
                 fractional = fractional)
      p_next <- pxt(tab, x = rep(60, length(times_A)) + times_A,
                     t = rep(1 / 4, length(times_A)),
                     fractional = fractional)
      expected_A <- sum((1 + tab@interest)^(-(times_A + 1 / 4) * power) *
        p_A * (1 - p_next))
      actual_A <- Axn(tab, x = 60, n = 12, m = 2, k = 4,
                      power = power, fractional = fractional)
      expect_equal(actual_A, expected_A, tolerance = 1e-10)
    }
  }
})

test_that("optimized APVs handle zero duration", {
  data(soaLt)
  tab <- with(soaLt, new("actuarialtable", interest = 0.06,
                           x = x, lx = Ix, name = "SOA2008"))

  expect_equal(axn(tab, x = 60, n = 0), 0)
  expect_equal(Axn(tab, x = 60, n = 0), 0)
  expect_equal(AExn(tab, x = 60, n = 0), 1)
})
