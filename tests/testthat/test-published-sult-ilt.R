library(testthat)
library(lifecontingencies)

context("Published life contingencies tables: SOA SULT (5%) and Illustrative Life Table (6%)")

# Reference values in this file are copied from published tables and are
# reproduced here with the package functions only.
#
# 1) Standard Ultimate Life Table (SULT), Society of Actuaries,
#    "LTAM Standard Ultimate Life Table" (2018), also Appendix D of
#    Dickson, Hardy & Waters, "Actuarial Mathematics for Life Contingent Risks".
#    https://www.soa.org/globalassets/assets/Files/Edu/2018/ltam-standard-ultimate-life-table.pdf
#    Basis: Makeham mu_x = A + B c^x, A = 0.00022, B = 2.7e-6, c = 1.124,
#    l_20 = 100000, i = 5%.
#
# 2) Illustrative Life Table (ILT), Bowers et al., "Actuarial Mathematics",
#    Appendix 2A, i = 6%; values as printed in the CAS Exam MAS-I tables
#    https://www.casact.org/sites/default/files/2021-03/masi_tables.pdf
#    From age 13 onwards the ILT follows Makeham with A = 0.0007,
#    B = 0.00005, c = 10^0.04. The package dataset soa08Act holds the ILT.

# Exn() and AExn() currently accept a scalar age only, hence sapply() below.
#
# A published figure printed with `digits` decimals must match the computed
# value once rounded, allowing for the half unit of rounding.
expect_published <- function(computed, published, digits) {
  tol <- 0.5 * 10^(-digits) + 1e-9
  diff <- abs(computed - published)
  expect_true(all(diff <= tol),
              info = paste0("max abs diff = ", signif(max(diff), 4),
                            " (tolerance ", tol, ")"))
}

sult_table <- function() {
  x <- 20:130
  A <- 0.00022; B <- 2.7e-6; c <- 1.124
  lx <- 100000 * exp(-A * (x - 20) - B / log(c) * (c^x - c^20))
  new("actuarialtable", x = x, lx = lx, interest = 0.05, name = "SOA SULT 5%")
}

# Columns of the SOA SULT, ages 20, 30, ..., 100.
sult_published <- data.frame(
  x    = c(20, 30, 40, 50, 60, 70, 80, 90, 100),
  lx   = c(100000.0, 99727.3, 99338.3, 98576.4, 96634.1, 91082.4, 75657.2, 41841.1, 6248.2),
  qx   = c(0.000250, 0.000315, 0.000527, 0.001209, 0.003398, 0.010413, 0.032658, 0.100917, 0.289584),
  ax   = c(19.9664, 19.3834, 18.4578, 17.0245, 14.9041, 12.0083, 8.5484, 5.1835, 2.7156),
  Ax   = c(0.04922, 0.07698, 0.12106, 0.18931, 0.29028, 0.42818, 0.59293, 0.75317, 0.87068),
  A2x  = c(0.00580, 0.01109, 0.02347, 0.05108, 0.10834, 0.21467, 0.38134, 0.58528, 0.76427),
  ax10 = c(8.0991, 8.0961, 8.0863, 8.0550, 7.9555, 7.6491, 6.7885, 4.9346, 2.7137),
  Ax10 = c(0.61433, 0.61447, 0.61494, 0.61643, 0.62116, 0.63576, 0.67674, 0.76502, 0.87078),
  ax20 = c(13.0559, 13.0410, 12.9935, 12.8428, 12.3816, 11.1109, 8.4639, 5.1833, 2.7156),
  Ax20 = c(0.37829, 0.37900, 0.38126, 0.38844, 0.41040, 0.47091, 0.59696, 0.75317, 0.87068),
  E5   = c(0.78252, 0.78219, 0.78113, 0.77772, 0.76687, 0.73295, 0.63365, 0.39659, 0.08777),
  E10  = c(0.61224, 0.61152, 0.60920, 0.60182, 0.57864, 0.50994, 0.33952, 0.09168, 0.00136),
  E20  = c(0.37440, 0.37254, 0.36663, 0.34824, 0.29508, 0.17313, 0.03113, 0.00012, 0.00000)
)

test_that("SULT: survival function and mortality rates match the SOA table", {
  tab <- sult_table()
  p <- sult_published
  expect_published(tab@lx[match(p$x, tab@x)], p$lx, 1)
  expect_published(qxt(tab, p$x, 1), p$qx, 6)
})

test_that("SULT: whole life annuity-due and insurance match the SOA table", {
  tab <- sult_table()
  p <- sult_published
  expect_published(axn(tab, x = p$x), p$ax, 4)
  expect_published(Axn(tab, x = p$x), p$Ax, 5)
  # second moment 2A_x = A_x at the doubled force of interest
  expect_published(Axn(tab, x = p$x, i = 1.05^2 - 1), p$A2x, 5)
})

test_that("SULT: 10 and 20 year temporary annuities and endowments match the SOA table", {
  tab <- sult_table()
  p <- sult_published
  expect_published(axn(tab, x = p$x, n = 10), p$ax10, 4)
  expect_published(axn(tab, x = p$x, n = 20), p$ax20, 4)
  expect_published(sapply(p$x, function(a) AExn(tab, x = a, n = 10)), p$Ax10, 5)
  expect_published(sapply(p$x, function(a) AExn(tab, x = a, n = 20)), p$Ax20, 5)
})

test_that("SULT: pure endowments match the SOA table", {
  tab <- sult_table()
  p <- sult_published
  expect_published(sapply(p$x, function(a) Exn(tab, x = a, n = 5)), p$E5, 5)
  expect_published(sapply(p$x, function(a) Exn(tab, x = a, n = 10)), p$E10, 5)
  expect_published(sapply(p$x, function(a) Exn(tab, x = a, n = 20)), p$E20, 5)
})

test_that("SULT: premiums and derived values agree with the published table", {
  # Textbook relations evaluated on the published (rounded) figures give
  # the reference; the tolerance reflects the rounding of the inputs.
  tab <- sult_table()
  p <- sult_published
  row <- function(age) p[p$x == age, ]
  r40 <- row(40); r60 <- row(60)

  # net annual premium, whole life, age 40, sum insured 100000
  P_pub <- 100000 * r40$Ax / r40$ax
  P_pkg <- 100000 * Axn(tab, 40) / axn(tab, 40)
  expect_equal(P_pkg, P_pub, tolerance = 1e-4)
  expect_equal(round(P_pkg, 2), 655.87)

  # net annual premium, 20 year endowment, age 40
  expect_equal(100000 * AExn(tab, 40, 20) / axn(tab, 40, 20),
               100000 * r40$Ax20 / r40$ax20, tolerance = 1e-4)

  # 20 year term insurance: A^1_{40:20} = A_{40:20} - 20E_40
  expect_equal(Axn(tab, 40, n = 20), r40$Ax20 - r40$E20, tolerance = 1e-4)

  # 20 year deferred whole life annuity-due: 20|a_40 = 20E_40 * a_60
  expect_equal(axn(tab, 40, m = 20), r40$E20 * r60$ax, tolerance = 1e-4)

  # whole life recursion across 20 years: A_40 = A^1_{40:20} + 20E_40 A_60
  expect_equal(Axn(tab, 40), (r40$Ax20 - r40$E20) + r40$E20 * r60$Ax,
               tolerance = 1e-3)

  # variance of the present value of a unit whole life benefit, age 60
  var_pub <- r60$A2x - r60$Ax^2
  var_pkg <- Axn(tab, 60, i = 1.05^2 - 1) - Axn(tab, 60)^2
  expect_equal(var_pkg, var_pub, tolerance = 1e-3)

  # annuity-insurance identity A_x = 1 - d a_x on published values
  d <- 0.05 / 1.05
  expect_published(1 - d * axn(tab, p$x), p$Ax, 5)
})

# Illustrative Life Table (i = 6%) ---------------------------------------

ilt_makeham_table <- function() {
  # Rebuilt from the Makeham law from age 13 onward, radix fixed at age 20
  # to the printed l_20 = 9,617,802 (radix 10 million at age 0).
  x <- 20:140
  A <- 0.0007; B <- 0.00005; c <- 10^0.04
  lx <- 9617802 * exp(-A * (x - 20) - B / log(c) * (c^x - c^20))
  new("actuarialtable", x = x, lx = lx, interest = 0.06, name = "ILT (Makeham) 6%")
}

# CAS MAS-I tables, ILT at i = 6% (1000 A_x etc. rescaled to unit values).
ilt_published <- data.frame(
  x   = c(20, 30, 40, 50, 60, 65),
  lx  = c(9617802, 9501381, 9313166, 8950901, 8188074, 7533964),
  qx1000 = c(1.03, 1.53, 2.78, 5.92, 13.76, 21.32),
  ax  = c(16.5133, 15.8561, 14.8166, 13.2668, 11.1454, 9.8969),
  Ax1000  = c(65.28, 102.48, 161.32, 249.05, 369.13, 439.80),
  A2x1000 = c(14.30, 25.31, 48.63, 94.76, 177.41, 236.03),
  E5_1000  = c(743.16, 740.91, 735.29, 721.37, 687.56, 656.23),
  E10_1000 = c(551.64, 547.33, 536.67, 510.81, 451.20, 399.94),
  E20_1000 = c(301.93, 293.74, 274.14, 230.47, 149.06, 97.60)
)

check_ilt <- function(tab, lx_scale) {
  p <- ilt_published
  # soa08Act stores l_x with radix 100000 and two decimals, hence the
  # relative tolerance instead of exact integer matching
  expect_equal(tab@lx[match(p$x, tab@x)] * lx_scale, p$lx, tolerance = 1e-6)
  expect_published(1000 * qxt(tab, p$x, 1), p$qx1000, 2)
  expect_published(axn(tab, x = p$x), p$ax, 4)
  expect_published(1000 * Axn(tab, x = p$x), p$Ax1000, 2)
  expect_published(1000 * Axn(tab, x = p$x, i = 1.06^2 - 1), p$A2x1000, 2)
  expect_published(1000 * sapply(p$x, function(a) Exn(tab, x = a, n = 5)), p$E5_1000, 2)
  expect_published(1000 * sapply(p$x, function(a) Exn(tab, x = a, n = 10)), p$E10_1000, 2)
  expect_published(1000 * sapply(p$x, function(a) Exn(tab, x = a, n = 20)), p$E20_1000, 2)

}

test_that("ILT: packaged soa08Act reproduces the published Illustrative Life Table", {
  data("soa08Act", package = "lifecontingencies", envir = environment())
  # soa08Act uses radix 100000 at age 0, the printed table uses 10 million
  check_ilt(soa08Act, lx_scale = 100)
})

test_that("ILT: table rebuilt from the Makeham law reproduces the published values", {
  check_ilt(ilt_makeham_table(), lx_scale = 1)
})

test_that("ILT: premiums from Bowers' basis agree with the published values", {
  data("soa08Act", package = "lifecontingencies", envir = environment())
  p <- ilt_published
  r <- function(age) p[p$x == age, ]
  # net annual premium, whole life, age 40, per 1000 of sum insured
  P40 <- 1000 * Axn(soa08Act, 40) / axn(soa08Act, 40)
  expect_equal(P40, r(40)$Ax1000 / r(40)$ax, tolerance = 1e-4)
  expect_equal(round(P40, 2), 10.89)
  # 20 year deferred annuity-due at 40: 20E_40 * a_60
  expect_equal(axn(soa08Act, 40, m = 20),
               r(40)$E20_1000 / 1000 * r(60)$ax, tolerance = 1e-4)
  # pure endowment chain: 20E_40 = 10E_40 * 10E_50
  expect_equal(Exn(soa08Act, 40, 20),
               r(40)$E10_1000 * r(50)$E10_1000 / 1e6, tolerance = 1e-4)
})
