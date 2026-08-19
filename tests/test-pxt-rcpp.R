library(lifecontingencies)
data(soa08Act)

# The public pxt() signature must remain unchanged.
stopifnot(identical(
  names(formals(pxt)),
  c("object", "x", "t", "fractional", "decrement")
))

reference_pxt <- function(object, x, t, fractional = "linear") {
  vapply(seq_len(max(length(x), length(t))), function(i) {
    lifecontingencies:::pxtold(
      object,
      x = x[(i - 1) %% length(x) + 1],
      t = t[(i - 1) %% length(t) + 1],
      fractional = fractional
    )
  }, numeric(1))
}

for (method in c("linear", "constant force", "hyperbolic")) {
  x <- seq(1, 100, by = 0.25)
  t <- seq(0.25, 20, by = 0.25)
  expected <- reference_pxt(soa08Act, x, t, method)
  code <- match(method, c("linear", "constant force", "hyperbolic")) - 1
  actual <- .pxtCpp(x, t, soa08Act@lx, getOmega(soa08Act), code)
  stopifnot(all.equal(actual, expected, tolerance = 1e-10))
}

# Scalar recycling and vector recycling must agree with the R API.
stopifnot(all.equal(
  .pxtCpp(c(30, 40, 50), 0.5, soa08Act@lx, getOmega(soa08Act), 0),
  pxt(soa08Act, x = c(30, 40, 50), t = 0.5),
  tolerance = 1e-10
))

# Fractional starting ages and times.
x <- c(30.25, 40.5, 65.75, 85.25)
t <- c(1.5, 2.25, 0.75, 5.5)
for (method in c("linear", "constant force", "hyperbolic")) {
  code <- match(method, c("linear", "constant force", "hyperbolic")) - 1
  expected <- pxt(soa08Act, x = x, t = t, fractional = method)
  actual <- .pxtCpp(x, t, soa08Act@lx, getOmega(soa08Act), code)
  stopifnot(all.equal(actual, expected, tolerance = 1e-10))
}
