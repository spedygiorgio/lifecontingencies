## markovchain is an optional dependency (Suggests). The tests check the two
## possible states of the installation, and that the package behaves as
## documented in each of them.

library(lifecontingencies)

mdtDf <- data.frame(x = c(0, 1, 2, 3), death = c(100, 50, 30, 10),
                    lapse = c(150, 20, 2, 0))
myMdt <- new("mdt", name = "example Mdt", table = mdtDf)

test_that("the package loads and its core functions work without markovchain", {
  expect_true(is.function(pxt))
  expect_equal(getOmega(myMdt), 3)
  expect_equal(getDecrements(myMdt), c("death", "lapse"))
  expect_equal(unname(dxt(myMdt, 0, 1, decrement = "death")), 100)
})

if (requireNamespace("markovchain", quietly = TRUE)) {

  test_that("markovchain installed: the coercions are registered and work", {
    expect_true(methods::existsMethod("coerce", c("mdt", "markovchainList")))
    expect_true(methods::existsMethod("coerce", c("lifetable", "markovchainList")))
    mcl <- as(myMdt, "markovchainList")
    expect_s4_class(mcl, "markovchainList")
    expect_equal(length(mcl@markovchains), getOmega(myMdt) + 1)
  })

  test_that("markovchain installed: rmdt() simulates and keeps the dimensions", {
    set.seed(1)
    sim <- rmdt(n = 5, object = myMdt, x = 0, t = 3, include.t0 = FALSE)
    expect_equal(dim(sim), c(3, 5))
    simT0 <- rmdt(n = 5, object = myMdt, x = 0, t = 3, include.t0 = TRUE)
    expect_equal(dim(simT0), c(4, 5))
  })

} else {

  test_that("markovchain missing: the coercions are not registered", {
    expect_false(methods::existsMethod("coerce", c("mdt", "markovchainList")))
    expect_false(methods::existsMethod("coerce", c("lifetable", "markovchainList")))
    expect_false(methods::canCoerce(myMdt, "markovchainList"))
    expect_error(as(myMdt, "markovchainList"), "markovchainList")
  })

  test_that("markovchain missing: rmdt() stops with an informative error", {
    expect_error(rmdt(n = 5, object = myMdt, x = 0, t = 3),
                 "requires the optional 'markovchain' package")
  })

}
