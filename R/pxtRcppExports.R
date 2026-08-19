.pxtCpp <- function(x, t, lx, omega, fractional_method) {
  .Call(`_lifecontingencies_pxtCpp`, x, t, lx, omega, fractional_method)
}
