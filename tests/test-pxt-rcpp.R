library(lifecontingencies)
data(soa08Act)

# The public pxt() signature must remain unchanged.
stopifnot(identical(
  names(formals(pxt)),
  c("object", "x", "t", "fractional", "decrement")
))

# Compare the native kernel with the public implementation.  Do not use the
# legacy pxtold() helper here: it is an internal compatibility implementation
# and is not part of the backend contract being tested.
reference_pxt <- function(object, x, t, fractional = "linear") {
  pxt(object, x = x, t = t, fractional = fractional)
}

for (method in c("linear", "constant force", "hyperbolic")) {
  x <- seq(1, 100, by = 0.25)
  t <- seq(0.25, 20, by = 0.25)
  expected <- reference_pxt(soa08Act, x, t, method)
  code <- match(method, c("linear", "constant force", "hyperbolic")) - 1
  actual <- lifecontingencies:::.pxtCpp(
    x, t, soa08Act@lx, getOmega(soa08Act), code
  )
  stopifnot(all.equal(actual, expected, tolerance = 1e-10))
}

# Scalar recycling and vector recycling must agree with the R API.
stopifnot(all.equal(
  lifecontingencies:::.pxtCpp(
    c(30, 40, 50), 0.5, soa08Act@lx, getOmega(soa08Act), 0
  ),
  pxt(soa08Act, x = c(30, 40, 50), t = 0.5),
  tolerance = 1e-10
))

# Fractional starting ages and times.
x <- c(30.25, 40.5, 65.75, 85.25)
t <- c(1.5, 2.25, 0.75, 5.5)
for (method in c("linear", "constant force", "hyperbolic")) {
  code <- match(method, c("linear", "constant force", "hyperbolic")) - 1
  expected <- pxt(soa08Act, x = x, t = t, fractional = method)
  actual <- lifecontingencies:::.pxtCpp(
    x, t, soa08Act@lx, getOmega(soa08Act), code
  )
  stopifnot(all.equal(actual, expected, tolerance = 1e-10))
}
