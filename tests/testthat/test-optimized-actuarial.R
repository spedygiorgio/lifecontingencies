test_that("optimized axn preserves the public signature", {
  expect_identical(
    names(formals(axn)),
    c("actuarialtable", "x", "n", "i", "m", "k", "type",
      "power", "payment", "...")
  )
  expect_identical(
    names(formals(Axn)),
    c("actuarialtable", "x", "n", "i", "m", "k", "type", "power", "...")
  )
  expect_identical(
    names(formals(AExn)),
    c("actuarialtable", "x", "n", "i", "k", "type", "power")
  )
})

test_that("optimized axn matches legacy implementation", {
  legacy <- Vectorize(lifecontingencies:::axnold, "x")
  x <- c(30, 45, 60, 75)
  expect_equal(
    axn(soa08Act, x = x, n = 20, k = 1, payment = "advance"),
    legacy(soa08Act, x = x, n = 20, k = 1, payment = "advance"),
    tolerance = 1e-12
  )
  expect_equal(
    axn(soa08Act, x = x, n = 20, k = 4, payment = "immediate"),
    vapply(x, function(xx) lifecontingencies:::axnold(
      soa08Act, x = xx, n = 20, k = 4, payment = "immediate"), numeric(1)),
    tolerance = 1e-11
  )
})

test_that("optimized Axn matches legacy implementation", {
  legacy <- Vectorize(lifecontingencies:::Axnold, "x")
  x <- c(30, 45, 60, 75)
  expect_equal(
    Axn(soa08Act, x = x, n = 20, k = 1),
    legacy(soa08Act, x = x, n = 20, k = 1),
    tolerance = 1e-12
  )
  expect_equal(
    Axn(soa08Act, x = x, n = 20, k = 4),
    vapply(x, function(xx) lifecontingencies:::Axnold(
      soa08Act, x = xx, n = 20, k = 4), numeric(1)),
    tolerance = 1e-11
  )
})

test_that("optimized AExn equals Axn plus Exn", {
  x <- c(30, 45, 60, 75)
  n <- 20
  expected <- vapply(x, function(xx)
    lifecontingencies:::Axnold(soa08Act, x = xx, n = n, k = 4) +
      Exn(soa08Act, x = xx, n = n, i = soa08Act@interest), numeric(1))
  expect_equal(
    AExn(soa08Act, x = x, n = n, k = 4),
    expected,
    tolerance = 1e-11
  )
})

test_that("fractional mortality assumptions agree with pxt/qxt", {
  x <- c(30.5, 45.25, 60.75)
  # k = 2, so contract terms must lie on the half-year payment grid.
  n <- c(10.5, 15.0, 20.5)

  for (fractional in c("linear", "constant force", "hyperbolic")) {
    expected_axn <- vapply(seq_along(x), function(j) {
      times <- seq(from = 0.5, to = n[j], by = 0.5)
      p <- pxt(soa08Act, x = x[j], t = times, fractional = fractional)
      presentValue(
        cashFlows = rep(0.5, length(times)),
        timeIds = times,
        interestRates = soa08Act@interest,
        probabilities = p
      )
    }, numeric(1))

    expect_equal(
      axn(soa08Act, x = x, n = n, k = 2,
          payment = "immediate", fractional = fractional),
      expected_axn,
      tolerance = 1e-10
    )
  }
})
