library(testthat)
library(lifecontingencies)

context("pxt(): native life-table kernel reproduces the former R code")

# Former R implementation of the life-table branch of pxt(), kept verbatim as
# the reference. The native kernel evaluates the same formula and returns the
# same doubles bit for bit on most platforms, but bit-for-bit equality is not
# portable: a compiler that contracts a * b + c into a fused multiply-add
# (Apple clang on aarch64, for one) rounds the intermediate product
# differently and shifts the result by a few ulps, under the "linear" and
# "hyperbolic" assumptions which are built from products and sums. What the
# kernel must reproduce exactly is the shape of the answer -- which entries
# are NaN and which are zero, i.e. the degenerate cases -- and the finite
# values to a far tighter tolerance than any actuarial use could notice.
pxt_reference <- function(object, x, t, fractional) {
  n <- max(length(t), length(x))
  t <- rep(t, length.out = n)
  x <- rep(x, length.out = n)
  floorx <- floor(x)
  eps_x <- x - floorx
  u <- t + eps_x
  flooru <- floor(u)
  eps_u <- u - flooru
  omega <- getOmega(object)
  mylx <- c(object@lx, 0)
  names(mylx) <- paste0("x", c(object@x, omega + 1))
  l_floorx <- mylx[paste0("x", floorx)]
  l_floorxp1 <- mylx[paste0("x", floorx + 1)]
  l_floorxu <- mylx[paste0("x", floorx + flooru)]
  l_floorxup1 <- mylx[paste0("x", floorx + flooru + 1)]
  flooru_p_floorx <- l_floorxu / l_floorx
  one_p_floorxu <- l_floorxup1 / l_floorxu
  one_p_floorx <- l_floorxp1 / l_floorx
  flooru_p_floorx[is.na(flooru_p_floorx)] <- 0
  one_p_floorxu[is.na(one_p_floorxu)] <- 0
  one_p_floorx[is.na(one_p_floorx)] <- 0
  if (fractional == "linear") {
    u_p_floorx <- flooru_p_floorx * (1 - eps_u * (1 - one_p_floorxu))
    eps_x_p_floorx <- 1 - eps_x * (1 - one_p_floorx)
  } else if (fractional == "constant force") {
    u_p_floorx <- flooru_p_floorx * one_p_floorxu^eps_u
    eps_x_p_floorx <- one_p_floorx^eps_x
  } else {
    u_p_floorx <- flooru_p_floorx * one_p_floorxu /
      (1 - (1 - eps_u) * (1 - one_p_floorxu))
    eps_x_p_floorx <- one_p_floorx / (1 - (1 - eps_x) * (1 - one_p_floorx))
  }
  as.numeric(u_p_floorx / eps_x_p_floorx)
}

# Relative tolerance of 1e-12: ~1e4 ulps of head room over the handful of ulps
# a contracted multiply-add can cost, still 1e4 times tighter than testthat's
# own default.
expectSamePxt <- function(got, want, info) {
  expect_identical(is.nan(got), is.nan(want), info = paste(info, "- NaN pattern"))
  expect_identical(got == 0, want == 0, info = paste(info, "- zeros"))
  expect_equal(got, want, tolerance = 1e-12, info = info)
}

data("soa08Act", package = "lifecontingencies", envir = environment())
data("AF92Lt", package = "lifecontingencies", envir = environment())
# a table that does not start at age 0
sult <- local({
  x <- 20:130
  lx <- 1e5 * exp(-0.00022 * (x - 20) - 2.7e-6 / log(1.124) * (1.124^x - 1.124^20))
  new("actuarialtable", x = x, lx = lx, interest = 0.05)
})
tables <- list(soa08Act = soa08Act, AF92Lt = AF92Lt, sult = sult)
methods <- c("linear", "constant force", "hyperbolic")

test_that("random ages and durations, all tables and fractional assumptions", {
  set.seed(42)
  N <- 5000
  x <- c(round(runif(N, 0, 140), sample(0:3, N, TRUE)), 0:140)
  t <- c(round(rexp(N, 1 / 15), sample(0:3, N, TRUE)),
         rep(c(0, 1, 1 / 12, 200), length.out = 141))
  for (nm in names(tables)) for (f in methods) {
    expectSamePxt(suppressWarnings(pxt(tables[[nm]], x, t, fractional = f)),
                  suppressWarnings(pxt_reference(tables[[nm]], x, t, f)),
                  info = paste(nm, f))
  }
})

test_that("boundaries: below the first age, at omega, beyond omega + 1", {
  for (nm in names(tables)) for (f in methods) {
    tb <- tables[[nm]]
    om <- getOmega(tb)
    x <- c(0, 0.5, pmax(0, tb@x[1] - c(1, 0.5)), tb@x[1], om - 1, om - 0.5, om, om + 0.5, om + 1, om + 3)
    for (tt in list(0, 0.5, 1, 1.5, 10, c(0, 0.25, 1, 2.5, 5, 7, 9, 11, 13, 20, 50))) {
      expectSamePxt(suppressWarnings(pxt(tb, x, tt, fractional = f)),
                    suppressWarnings(pxt_reference(tb, x, tt, f)),
                    info = paste(nm, f))
    }
  }
})

test_that("recycling of x and t is unchanged", {
  expectSamePxt(pxt(soa08Act, 40, 0:30), pxt_reference(soa08Act, 40, 0:30, "linear"),
                info = "recycled t")
  expectSamePxt(pxt(soa08Act, 20:50, 10), pxt_reference(soa08Act, 20:50, 10, "linear"),
                info = "recycled x")
  expectSamePxt(qxt(soa08Act, c(30, 40.5), c(1, 2.5)),
                1 - pxt_reference(soa08Act, c(30, 40.5), c(1, 2.5), "linear"),
                info = "qxt")
})
