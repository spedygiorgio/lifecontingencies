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
      expected_ax <- axnold(tab, x = 60, n = 12, m = 2, k = 4,
                            power = power)
      actual_ax <- axn(tab, x = 60, n = 12, m = 2, k = 4,
                       power = power, fractional = fractional)
      expect_equal(actual_ax, expected_ax, tolerance = 1e-10)

      expected_A <- Axnold(tab, x = 60, n = 12, m = 2, k = 4,
                            power = power)
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
