library(lifecontingencies)

context("Commutation functions vs direct calculation")
data(soa08Act)

## ---------------------------------------------------------------------------
## Tavola di commutazione ricavata dall'oggetto actuarialtable.
## as(soa08Act, "data.frame") chiama internamente .createActuarialTableCols,
## quindi questi test verificano anche la versione (eventualmente vettorizzata)
## dei cumulati Nx / Mx contro il calcolo diretto delle funzioni standard.
## L'interesse usato e' quello dell'oggetto (soa08Act@interest = 0.06):
## per coerenza NON sovrascriviamo 'i' nelle funzioni chiamate.
## ---------------------------------------------------------------------------
comm <- as(soa08Act, "data.frame")   # colonne: x, lx, Dx, Nx, Cx, Mx, Rx

## accessori scalari per eta'; ritornano 0 oltre la tavola
Dx <- function(age) { idx <- match(age, comm$x); if (is.na(idx)) 0 else comm$Dx[idx] }
Nx <- function(age) { idx <- match(age, comm$x); if (is.na(idx)) 0 else comm$Nx[idx] }
Mx <- function(age) { idx <- match(age, comm$x); if (is.na(idx)) 0 else comm$Mx[idx] }

tol <- 1e-6   # differenze attese solo dall'ordine di somma (floating point)

## ===========================================================================
## axn  -- rendite vitalizie / temporanee
##   a-due_x        = N_x / D_x
##   a-due_{x:n}    = (N_x - N_{x+n}) / D_x
##   a_x (post.)    = N_{x+1} / D_x
## ===========================================================================
test_that("axn (annuities) vs commutation functions", {
  # rendita vitalizia anticipata
  expect_equal(axn(soa08Act, x = 30, payment = "due"),
               Nx(30) / Dx(30), tolerance = tol)

  # rendita temporanea anticipata, 20 anni
  expect_equal(axn(soa08Act, x = 40, n = 20, payment = "due"),
               (Nx(40) - Nx(60)) / Dx(40), tolerance = tol)

  # rendita vitalizia posticipata
  expect_equal(axn(soa08Act, x = 50, payment = "immediate"),
               Nx(51) / Dx(50), tolerance = tol)
})

## ===========================================================================
## Axn  -- assicurazioni caso morte
##   A_x            = M_x / D_x
##   A^1_{x:n}      = (M_x - M_{x+n}) / D_x
## ===========================================================================
test_that("Axn (life insurance) vs commutation functions", {
  # vita intera
  expect_equal(Axn(soa08Act, x = 30),
               Mx(30) / Dx(30), tolerance = tol)

  # temporanea caso morte, 20 anni
  expect_equal(Axn(soa08Act, x = 45, n = 20),
               (Mx(45) - Mx(65)) / Dx(45), tolerance = tol)

  # temporanea caso morte, 40 anni
  expect_equal(Axn(soa08Act, x = 25, n = 40),
               (Mx(25) - Mx(65)) / Dx(25), tolerance = tol)
})

## ===========================================================================
## Exn  -- capitale differito (pure endowment)
##   nE_x = D_{x+n} / D_x
## ===========================================================================
test_that("Exn (pure endowment) vs commutation functions", {
  expect_equal(Exn(soa08Act, x = 30, n = 35),
               Dx(65) / Dx(30), tolerance = tol)

  expect_equal(Exn(soa08Act, x = 40, n = 25),
               Dx(65) / Dx(40), tolerance = tol)

  expect_equal(Exn(soa08Act, x = 50, n = 10),
               Dx(60) / Dx(50), tolerance = tol)
})

## ===========================================================================
## AExn -- assicurazione mista (endowment)
##   A_{x:n} = (M_x - M_{x+n} + D_{x+n}) / D_x
## ===========================================================================
test_that("AExn (endowment insurance) vs commutation functions", {
  expect_equal(AExn(soa08Act, x = 35, n = 30),
               (Mx(35) - Mx(65) + Dx(65)) / Dx(35), tolerance = tol)

  expect_equal(AExn(soa08Act, x = 40, n = 25),
               (Mx(40) - Mx(65) + Dx(65)) / Dx(40), tolerance = tol)

  # coerenza interna: AExn = Axn (temporanea morte) + Exn (capitale differito)
  expect_equal(AExn(soa08Act, x = 45, n = 20),
               Axn(soa08Act, x = 45, n = 20) + Exn(soa08Act, x = 45, n = 20),
               tolerance = tol)
})
