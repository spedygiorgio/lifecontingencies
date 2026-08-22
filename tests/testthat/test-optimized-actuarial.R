test_that("optimized actuarial functions preserve the public signatures", {
  expect_identical(names(formals(axn)), c("actuarialtable", "x", "n", "i", "m", "k", "type", "power", "payment", "..."))
  expect_identical(names(formals(Axn)), c("actuarialtable", "x", "n", "i", "m", "k", "type", "power", "..."))
  expect_identical(names(formals(AExn)), c("actuarialtable", "x", "n", "i", "k", "type", "power"))
})

test_that("optimized axn matches legacy implementation for payment aliases", {
  x <- c(30, 45, 60, 75)
  for (payment in c("advance", "due", "immediate", "arrears")) {
    expected <- vapply(x, function(xx) lifecontingencies:::axnold(
      soa08Act, x = xx, n = 20, m = 2, k = 4, payment = payment
    ), numeric(1))
    expect_equal(axn(soa08Act, x = x, n = 20, m = 2, k = 4, payment = payment),
                 expected, tolerance = 1e-11)
  }
})

test_that("optimized axn handles vectorized x, n and m", {
  x <- c(30, 45, 60, 75)
  n <- c(10, 15, 20, 25)
  m <- c(0, 1, 2, 3)
  for (payment in c("due", "immediate")) {
    expected <- vapply(seq_along(x), function(j) lifecontingencies:::axnold(
      soa08Act, x = x[j], n = n[j], m = m[j], k = 4, payment = payment
    ), numeric(1))
    expect_equal(axn(soa08Act, x = x, n = n, m = m, k = 4, payment = payment),
                 expected, tolerance = 1e-11)
  }
})

test_that("optimized Axn matches legacy implementation", {
  x <- c(30, 45, 60, 75)
  for (k in c(1, 2, 4, 12)) {
    expected <- vapply(x, function(xx) lifecontingencies:::Axnold(
      soa08Act, x = xx, n = 20, m = 2, k = k
    ), numeric(1))
    expect_equal(Axn(soa08Act, x = x, n = 20, m = 2, k = k),
                 expected, tolerance = 1e-11)
  }
})

test_that("optimized Axn handles vectorized x, n and m", {
  x <- c(30, 45, 60, 75)
  n <- c(10, 15, 20, 25)
  m <- c(0, 1, 2, 3)
  expected <- vapply(seq_along(x), function(j) lifecontingencies:::Axnold(
    soa08Act, x = x[j], n = n[j], m = m[j], k = 4
  ), numeric(1))
  expect_equal(Axn(soa08Act, x = x, n = n, m = m, k = 4),
               expected, tolerance = 1e-11)
})

test_that("optimized AExn equals historical Axn plus Exn", {
  x <- c(30, 45, 60, 75)
  n <- 20
  for (k in c(1, 2, 4, 12)) {
    expected <- vapply(x, function(xx) lifecontingencies:::Axnold(
      soa08Act, x = xx, n = n, k = k
    ) + Exn(soa08Act, x = xx, n = n, i = soa08Act@interest), numeric(1))
    expect_equal(AExn(soa08Act, x = x, n = n, k = k), expected,
                 tolerance = 1e-11)
  }
})

test_that("power and interest arguments are preserved", {
  x <- c(30, 45, 60)
  for (power in c(1, 2)) {
    expect_equal(
      axn(soa08Act, x = x, n = 15, k = 4, i = 0.04, power = power),
      vapply(x, function(xx) lifecontingencies:::axnold(
        soa08Act, x = xx, n = 15, k = 4, i = 0.04, power = power
      ), numeric(1)), tolerance = 1e-11
    )
    expect_equal(
      Axn(soa08Act, x = x, n = 15, k = 4, i = 0.04, power = power),
      vapply(x, function(xx) lifecontingencies:::Axnold(
        soa08Act, x = xx, n = 15, k = 4, i = 0.04, power = power
      ), numeric(1)), tolerance = 1e-11
    )
  }
})

test_that("fractional mortality assumptions agree with pxt/qxt", {
  x <- c(30.5, 45.25, 60.75)
  n <- c(10.5, 15.0, 20.5)
  for (fractional in c("linear", "constant force", "hyperbolic")) {
    expected_axn <- vapply(seq_along(x), function(j) {
      times <- seq(0.5, n[j], by = 0.5)
      p <- pxt(soa08Act, x = x[j], t = times, fractional = fractional)
      presentValue(rep(0.5, length(times)), times, soa08Act@interest, p)
    }, numeric(1))
    expect_equal(
      axn(soa08Act, x = x, n = n, k = 2, payment = "immediate", fractional = fractional),
      expected_axn, tolerance = 1e-10
    )

    expected_Axn <- vapply(seq_along(x), function(j) {
      times <- seq(0, n[j] - 0.5, by = 0.5)
      p <- pxt(soa08Act, x = x[j], t = times, fractional = fractional)
      q <- qxt(soa08Act, x = x[j] + times, t = 0.5, fractional = fractional)
      presentValue(rep(1, length(times)), times + 0.5, soa08Act@interest, p * q)
    }, numeric(1))
    expect_equal(
      Axn(soa08Act, x = x, n = n, k = 2, fractional = fractional),
      expected_Axn, tolerance = 1e-10
    )
  }
})

test_that("zero-term and default-term edge cases remain valid", {
  expect_equal(axn(soa08Act, x = 65, n = 0), 0)
  expect_equal(Axn(soa08Act, x = 65, n = 0), 0)
  expect_equal(AExn(soa08Act, x = 65, n = 0), Exn(soa08Act, x = 65, n = 0))
  x <- c(60, 65, 70)
  expect_equal(
    axn(soa08Act, x = x, k = 1),
    vapply(x, function(xx) lifecontingencies:::axnold(
      soa08Act, x = xx, payment = "advance"
    ), numeric(1)), tolerance = 1e-11
  )
})

test_that("stochastic paths remain available", {
  expect_no_error(axn(soa08Act, x = 65, n = 5, type = "ST"))
  expect_no_error(Axn(soa08Act, x = 65, n = 5, type = "ST"))
  expect_no_error(AExn(soa08Act, x = 65, n = 5, type = "ST"))
})
