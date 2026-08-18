#' Autoplot a life table
#'
#' Produces a ggplot2 representation of the number of survivors by age.
#' The method is defined for `lifetable` and is therefore also available
#' for subclasses such as `actuarialtable` through S4 inheritance.
#'
#' @param object A `lifetable` object, or an object inheriting from it.
#' @param ... Additional arguments passed to `ggplot2::geom_line()`.
#'
#' @return A `ggplot2` plot object.
#' @importFrom ggplot2 autoplot ggplot aes geom_line labs
#' @export
setMethod(
  "autoplot",
  signature(object = "lifetable"),
  definition = function(object, ...) {
    data <- data.frame(x = object@x, lx = object@lx)

    ggplot2::ggplot(data, ggplot2::aes(x = x, y = lx)) +
      ggplot2::geom_line(...) +
      ggplot2::labs(
        title = paste("Life table", object@name),
        x = "Age",
        y = "Number of survivors"
      )
  }
)
