#' Autoplot a life table
#'
#' Produces a ggplot2 representation of the number of survivors by age.
#' The method is defined for `lifetable`; S3 dispatch also makes it
#' available for S4 subclasses such as `actuarialtable`.
#'
#' @param object A `lifetable` object, or an object inheriting from it.
#' @param ... Additional arguments passed to `ggplot2::geom_line()`.
#'
#' @return A `ggplot2` plot object.
#' @importFrom ggplot2 autoplot
#' @export
#' @name autoplot.lifetable
#' @rdname autoplot.lifetable
#' @aliases autoplot.lifetable
#' @method autoplot lifetable
#' @examples
#' lt <- new("lifetable", x = 0:3, lx = c(100, 90, 50, 10))
#' autoplot(lt)
autoplot.lifetable <- function(object, ...) {
  data <- data.frame(x = object@x, lx = object@lx)

  ggplot2::ggplot(
    data,
    ggplot2::aes(x = data[["x"]], y = data[["lx"]])
  ) +
    ggplot2::geom_line(...) +
    ggplot2::labs(
      title = paste("Life table", object@name),
      x = "Age",
      y = "Number of survivors"
    )
}
