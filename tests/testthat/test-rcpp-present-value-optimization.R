test_that("presentValueC preserves integer power special cases", {
  cash_flows <- c(-10, 20, 35, -50)
  times <- c(0.5, 1, 2, 4)
  rates <- c(0.01, 0.02, 0.03, 0.04)
  probabilities <- c(1, 0.95, 0.9, 0.8)

  reference <- function(power) {
    v <- (1 + rates)^(-times)
    sum((cash_flows^power) * (v^power) * probabilities)
  }

  expect_equal(
    lifecontingencies:::.presentValueC(
      cash_flows, times, rates, probabilities, 2
    ),
    reference(2),
    tolerance = 1e-12
  )

  expect_equal(
    lifecontingencies:::.presentValueC(
      cash_flows, times, rates, probabilities, 3
    ),
    reference(3),
    tolerance = 1e-12
  )
})
