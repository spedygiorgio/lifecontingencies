library(testthat)

reference_present_value <- function(cashFlows, timeIds, interestRates,
                                    probabilities, power = 1) {
  v <- (1 + interestRates)^(-timeIds)
  sum((cashFlows^power) * (v^power) * probabilities)
}

test_that("presentValueC matches the historical R implementation", {
  cases <- list(
    list(c(100, -30, 50, 70), c(0, 0.5, 3, 6.25), 0.035,
         c(1, 0.9, 0.8, 0.65), 1),
    list(c(10, 10, 10, 10, 10, 10, 110), 1:7,
         c(0.02, 0.021, 0.0225, 0.024, 0.0255, 0.027, 0.028),
         rep(0.995, 7), 1),
    list(c(3, 5, 7), c(1, 2, 3), c(0.01, 0.015, 0.02),
         c(0.9, 0.8, 0.7), 2),
    list(c(-3, 5, -7), c(1, 2, 3), c(0.01, 0.015, 0.02),
         c(0.9, 0.8, 0.7), 3)
  )

  for (case in cases) {
    names(case) <- c("cashFlows", "timeIds", "interestRates",
                     "probabilities", "power")
    expected <- do.call(reference_present_value, case)
    rates <- rep(case$interestRates, length.out = length(case$timeIds))
    case$interestRates <- rates
    actual <- do.call(lifecontingencies:::.presentValueC, case)
    expect_equal(actual, expected, tolerance = 1e-10)
  }
})

test_that("presentValueC rejects incompatible lengths", {
  expect_error(
    lifecontingencies:::.presentValueC(c(1, 2), c(1, 2), c(0.03), c(1, 1))
  )
})
