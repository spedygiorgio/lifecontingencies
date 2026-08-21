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
  n <- c(10.5, 15.25, 20.75)
  for (fractional in c("linear", "constant force", "hyperbolic")) {
    expected_axn <- vapply(seq_along(x), function(j) {
      times <- seq(from = 1 / 2, to = n[j], by = 1 / 2)
      p <- pxt(soa08Act, x = x[j], t = times, fractional = fractional)
      presentValue(rep(1 / 2, length(times)), times,
                   soa08Act@interest, p)
    }, numeric(1))
    # The public axn API accepts fractional assumptions through ... .
    expect_equal(
      axn(soa08Act, x = x, n = n, k = 2,
          payment = "immediate", fractional = fractional),
      expected_axn,
      tolerance = 1e-10
    )
  }
})
