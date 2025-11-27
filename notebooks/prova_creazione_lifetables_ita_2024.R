require(lifecontingencies)
require(data.table)

# load the ../data/ita2023.rda
data("ita2023")

ita2023_lt <- new("lifetable",
                      x = ita2023$x,
                      lx = ita2023$lx,
                      name = "istat_italy_2023")



getOmega(ita2023_lt)


# start writing some tests 

require(testthat)

## expect that getOmega == 110
stopifnot(getOmega(ita2023_lt) == 110)

## expect that dx1 == 21
dxt(ita2023_lt,x=1, t=1)  # 21

test_that("ita2023 lifetable object can be constructed", {
  data("ita2023", package = "lifecontingencies")
  
  ita2023_lt <- new("lifetable",
                    x    = ita2023$x,
                    lx   = ita2023$lx,
                    name = "istat_italy_2023")
  
  expect_s4_class(ita2023_lt, "lifetable")
  
  # omega atteso (come hai verificato a mano)
  expect_equal(getOmega(ita2023_lt), 110)
  
  # dx_1^1 atteso (come da tuo controllo: 2)
  expect_equal(dxt(ita2023_lt, x = 1, t = 1), 20)
  
  # check that expected curtated lifetime at birth is 82.5318 (rounded)
  expect_equal(exn(ita2023_lt, x = 0), 82.5318, tolerance = 1e-4)
})


