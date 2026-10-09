## dxt() on lifetable/actuarialtable objects is vectorised over x and t and
## looks ages up with match(). Reference: the scalar definition
##   d(x, t) = l_x - l_{x+t}              (integer t, l = 0 beyond omega)
##   d(x, k + f) = d(x, k) + f * d(x + k, 1)   (fractional t, UDD)
tb <- new("lifetable", x = 0:4, lx = c(1000, 900, 700, 300, 0), name = "toy")

test_that("dxt matches the scalar definition", {
  expect_equal(dxt(tb, 0, 1), 100)
  expect_equal(dxt(tb, 1, 2), 600)
  expect_equal(dxt(tb, 2), 400)                    # default t = 1
  expect_equal(dxt(tb, 0, 0.5), 50)
  expect_equal(dxt(tb, 1, 1.5), 200 + 0.5 * 400)
})

test_that("dxt is vectorised over x and t, with recycling", {
  expect_equal(dxt(tb, 0:2, 1), c(100, 200, 400))
  expect_equal(dxt(tb, 0, 1:3), c(100, 300, 700))
  expect_equal(dxt(tb, c(0, 1), c(1, 2.5)),
               c(dxt(tb, 0, 1), dxt(tb, 1, 2.5)))
  g <- expand.grid(x = 0:3, t = c(0, 1, 2, 0.25, 1.75, 6))
  expect_equal(dxt(tb, g$x, g$t),
               mapply(function(x, t) dxt(tb, x, t), g$x, g$t))
})

test_that("no deaths are counted beyond the last age", {
  expect_equal(dxt(tb, 3, 10), 300)
  expect_equal(dxt(tb, 3, 2.75), 300)       # 300 + 0.75 * d(5, 1) with d = 0
  expect_equal(dxt(tb, 4, 2.5), 0)
})

test_that("sum of yearly deaths equals the radix on a real table", {
  data(soa08Act)
  om <- getOmega(soa08Act)
  expect_equal(sum(dxt(soa08Act, 0:om, 1)), soa08Act@lx[1])
  expect_equal(dxt(soa08Act, 30, 5), soa08Act@lx[31] - soa08Act@lx[36])
})
