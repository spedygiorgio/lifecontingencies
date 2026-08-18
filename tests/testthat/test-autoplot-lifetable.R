library(lifecontingencies)

test_that("autoplot returns a ggplot for lifetable", {
  skip_if_not_installed("ggplot2")

  tbl <- new("lifetable", x = 0:3, lx = c(100, 90, 50, 10))
  p <- ggplot2::autoplot(tbl)

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Age")
  expect_equal(p$labels$y, "Number of survivors")
})

test_that("autoplot is inherited by actuarialtable", {
  skip_if_not_installed("ggplot2")

  tbl <- new(
    "actuarialtable",
    x = 0:3,
    lx = c(100, 90, 50, 10),
    interest = 0.03
  )
  p <- ggplot2::autoplot(tbl)

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, paste("Life table", tbl@name))
})
