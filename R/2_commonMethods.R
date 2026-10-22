#############################################################################
###
###         common methods
###


#' Last attainable age of a life or multiple-decrement table
#'
#' Return the \eqn{\omega} value of a life table or multiple-decrement
#' table, i.e. the largest age tabulated in the object. Methods are
#' provided for \code{\linkS4class{lifetable}} (inherited by
#' \code{\linkS4class{actuarialtable}}) and \code{\linkS4class{mdt}}.
#'
#' @param object A life-table-like object
#'   (\code{\linkS4class{lifetable}}, \code{\linkS4class{actuarialtable}}
#'   or \code{\linkS4class{mdt}}).
#'
#' @return A numeric scalar: the largest tabulated age.
#' @references Actuarial Mathematics (Second Edition), 1997, by Bowers,
#'   N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
#' @author Giorgio A. Spedicato
#' @seealso \code{\linkS4class{lifetable}}, \code{\linkS4class{actuarialtable}}
#' @examples
#' # Assumes the SOA example life table to be loaded
#' data(soaLt)
#' soa08 <- with(soaLt, new("lifetable", x = x, lx = Ix, name = "SOA2008"))
#' getOmega(soa08)
#' @export
setGeneric("getOmega", function(object) standardGeneric("getOmega"))

#' @rdname getOmega
#' @export
setMethod("getOmega","mdt",
		# For multiple decrement tables, omega is the largest age stored in table$x.
		function(object) {
			max(object@table$x)
		}
)


#' @rdname getOmega
#' @export
setMethod("getOmega","lifetable",
		# For life tables, omega is the largest age stored in slot x.
		function(object) {
			max(object@x)
		}
)
