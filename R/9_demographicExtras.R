#############################################################################
###
###   Extra demographic functions on life tables:
###   dispersion of the future lifetime (variance / standard deviation)
###   and distributional summaries of the age at death
###   (median, quantiles, modal age), exposed through standard R generics
###   where one exists.
###

#' Variance and standard deviation of the future lifetime
#'
#' \code{varxn} and \code{sdxn} return the variance and the standard deviation
#' of the future lifetime of a life aged \eqn{x}, as the natural second-moment
#' companions of \code{\link{exn}} (which returns the mean).
#'
#' @param object A \code{lifetable} or \code{actuarialtable} object.
#' @param x Attained age. Defaults to \code{0}.
#' @param n Length (in years) of a temporary period. If missing, the whole
#'   remaining lifespan is used.
#' @param type Either \code{"Kx"}/\code{"curtate"} for the curtate future
#'   lifetime \eqn{K_x} or \code{"Tx"}/\code{"complete"}/\code{"continuous"}
#'   for the complete future lifetime \eqn{T_x} (can be abbreviated). Default
#'   is \code{"Kx"}.
#'
#' @details
#' For the curtate future lifetime the (temporary) variance is computed exactly
#' from the life table as
#' \deqn{\mathrm{Var}(\min(K_x, n)) = \sum_{k=1}^{n}(2k-1)\,{}_k p_x - \left(\sum_{k=1}^{n}{}_k p_x\right)^2 .}
#'
#' For the complete future lifetime the variance is returned only for the whole
#' remaining lifespan (\code{n} missing), using the uniform-distribution-of-deaths
#' relation \eqn{\mathrm{Var}(T_x) = \mathrm{Var}(K_x) + 1/12} (because, under UDD,
#' \eqn{T_x = K_x + U} with \eqn{U\sim\mathrm{Unif}(0,1)} independent of \eqn{K_x}).
#' A temporary complete variance is not provided; use \code{type = "Kx"} for a
#' temporary period.
#'
#' @return A numeric value: the variance (\code{varxn}) or standard deviation
#'   (\code{sdxn}) of the (temporary) future lifetime.
#' @author Giorgio Alfredo Spedicato
#' @references Dickson, D.C.M., Hardy, M.R., Waters, H.R. (2013), Actuarial
#'   Mathematics for Life Contingent Risks, 2nd ed., Cambridge University Press.
#' @seealso \code{\link{exn}}
#' @examples
#' data(soa08Act)
#' # variance and sd of the curtate future lifetime at birth
#' varxn(soa08Act, x = 0)
#' sdxn(soa08Act, x = 0)
#' # complete future lifetime: Var(T) = Var(K) + 1/12
#' varxn(soa08Act, x = 65, type = "complete")
#' # temporary (20-year) curtate future lifetime at age 40
#' varxn(soa08Act, x = 40, n = 20)
#' @export
varxn <- function(object, x, n, type = "Kx") {
	if (!is(object, "lifetable"))
		stop("Error! Need lifetable or actuarialtable objects")
	if (missing(x)) x <- 0
	if (any(x < 0)) stop("Check x domain")
	type <- testtypelifearg(type)
	omega <- getOmega(object)
	fullLife <- missing(n)
	if (fullLife) n <- omega - x + 1
	if (n <= 0) return(0)

	# curtate future lifetime capped at n: K* = min(K_x, n)
	kpx <- pxt(object, x, 1:n)
	m1 <- sum(kpx)                       # E[K*]
	m2 <- sum((2 * (1:n) - 1) * kpx)     # E[K*^2]
	varK <- m2 - m1^2

	if (type == "Kx") return(varK)

	# complete future lifetime (type == "Tx"): whole remaining lifespan only
	if (!fullLife && (x + n <= omega))
		stop("variance of the complete future lifetime is only implemented for the whole remaining lifespan; use type = 'Kx' for a temporary period")
	# Var(T_x) = Var(K_x) + 1/12 under the uniform distribution of deaths
	return(varK + 1/12)
}

#' @rdname varxn
#' @export
sdxn <- function(object, x, n, type = "Kx") {
	sqrt(varxn(object, x = if (missing(x)) 0 else x,
	           n = if (missing(n)) getOmega(object) - (if (missing(x)) 0 else x) + 1 else n,
	           type = type))
}

## ---------------------------------------------------------------------
## Distributional summaries of the age at death, via standard R generics.
## ---------------------------------------------------------------------

# Quantiles of the age-at-death distribution, conditional on being alive at
# `age`. Linear interpolation on the l_x column (i.e. assuming a uniform
# distribution of deaths within each year of age).
.ageAtDeathQuantile <- function(object, probs, age) {
	idx <- which(object@x >= age)
	if (length(idx) < 2L) stop("Not enough ages above 'age' to compute quantiles")
	xs <- object@x[idx]
	lx <- object@lx[idx]
	l0 <- lx[1]
	nL <- length(lx)
	vapply(probs, function(p) {
		if (is.na(p) || p < 0 || p > 1) return(NA_real_)
		if (p <= 0) return(xs[1])
		target <- (1 - p) * l0            # l_x value reached at the quantile
		if (target <= lx[nL]) return(xs[nL])   # quantile beyond the last age
		j <- max(which(lx > target))      # last age with l_x strictly above target
		# interpolate the age at which l_x equals target, between xs[j] and xs[j+1]
		xs[j] + (lx[j] - target) / (lx[j] - lx[j + 1])
	}, numeric(1))
}

#' @importFrom stats median quantile
NULL

#' Median age at death of a life table
#'
#' S4 method for the \code{\link[stats]{median}} generic: the median age at
#' death implied by a \code{lifetable}, i.e. the age at which the survivorship
#' column \eqn{l_x} has fallen to half of its value at \code{age} (linear
#' interpolation on \eqn{l_x}).
#'
#' @param x A \code{lifetable} or \code{actuarialtable} object.
#' @param na.rm Unused, kept for compatibility with the generic.
#' @param ... Optional \code{age} (default: the youngest tabulated age): the
#'   median is computed for the age at death conditional on being alive at
#'   \code{age}.
#' @return The median age at death (a numeric value).
#' @seealso \code{\link{quantile}}, \code{\link{modalAge}}, \code{\link{exn}}
#' @examples
#' data(soa08Act)
#' median(soa08Act)              # median age at death from birth
#' median(soa08Act, age = 65)    # median age at death given survival to 65
#' @usage \S4method{median}{lifetable}(x, na.rm = FALSE, ...)
#' @aliases median,lifetable-method
#' @exportMethod median
setGeneric("median")
setMethod("median", signature(x = "lifetable"),
	function(x, na.rm = FALSE, ...) {
		dots <- list(...)
		age <- if (!is.null(dots$age)) dots$age else min(x@x)
		unname(.ageAtDeathQuantile(x, 0.5, age))
	}
)

#' Quantiles of the age-at-death distribution of a life table
#'
#' S4 method for the \code{\link[stats]{quantile}} generic: quantiles of the
#' age at death implied by a \code{lifetable}, obtained by linear interpolation
#' on the survivorship column \eqn{l_x}.
#'
#' @param x A \code{lifetable} or \code{actuarialtable} object.
#' @param probs Numeric vector of probabilities in \eqn{[0, 1]}.
#' @param age Youngest age to condition on (default: the youngest tabulated
#'   age). Quantiles refer to the age at death given survival to \code{age}.
#' @param names Logical: if \code{TRUE} (default) the result is named with the
#'   probabilities, as \code{\link[stats]{quantile}} does.
#' @param ... Further arguments (currently unused).
#' @return A numeric vector of age-at-death quantiles.
#' @seealso \code{\link{median}}, \code{\link{modalAge}}
#' @examples
#' data(soa08Act)
#' quantile(soa08Act)
#' quantile(soa08Act, probs = c(0.1, 0.9), age = 65)
#' @usage \S4method{quantile}{lifetable}(x, probs = seq(0, 1, 0.25), age = min(x@x), names = TRUE, ...)
#' @aliases quantile,lifetable-method
#' @exportMethod quantile
setGeneric("quantile")
setMethod("quantile", signature(x = "lifetable"),
	function(x, probs = seq(0, 1, 0.25), age = min(x@x), names = TRUE, ...) {
		out <- .ageAtDeathQuantile(x, probs, age)
		if (names) names(out) <- paste0(format(100 * probs, trim = TRUE), "%")
		out
	}
)

#' Modal age at death of a life table
#'
#' Returns the age at which the number of deaths \eqn{d_x} is largest, i.e. the
#' Lexis modal age at death. (R's base \code{\link[base]{mode}} returns the
#' storage type of an object and cannot be used for this, so a dedicated
#' function is provided.)
#'
#' @param object A \code{lifetable} or \code{actuarialtable} object.
#' @param startAge Optional lower age bound for the search. Use it (e.g.
#'   \code{startAge = 10}) to obtain the adult modal age at death, ignoring the
#'   infant-mortality peak.
#' @param interpolate Logical. If \code{TRUE}, a continuous estimate is
#'   returned by fitting a parabola through the \eqn{d_x} values at the modal
#'   age and its two neighbours (requires unit age spacing and an interior
#'   mode). Default \code{FALSE} (the integer age of maximum \eqn{d_x}).
#' @return The modal age at death (a numeric value).
#' @details The death counts are \eqn{d_x = l_x - l_{x+1}}; the artificial mass
#'   at the last, open age interval (all remaining survivors) is excluded from
#'   the search.
#' @seealso \code{\link{median}}, \code{\link{quantile}}
#' @examples
#' data(soa08Act)
#' modalAge(soa08Act)
#' modalAge(soa08Act, startAge = 10, interpolate = TRUE)
#' @export
modalAge <- function(object, startAge = NULL, interpolate = FALSE) {
	if (!is(object, "lifetable"))
		stop("Error! Need lifetable or actuarialtable objects")
	xs <- object@x
	lx <- object@lx
	nAll <- length(xs)
	dx <- lx[-nAll] - lx[-1]      # deaths at ages xs[1 .. nAll-1]
	xsD <- xs[-nAll]
	keep <- if (is.null(startAge)) seq_along(xsD) else which(xsD >= startAge)
	if (length(keep) == 0L) stop("No ages at or above 'startAge'")
	xsk <- xsD[keep]
	dxk <- dx[keep]
	i <- which.max(dxk)
	M <- xsk[i]
	if (!interpolate) return(M)
	if (i == 1L || i == length(dxk)) return(M)   # mode at an edge: no parabola
	dm1 <- dxk[i - 1]; d0 <- dxk[i]; dp1 <- dxk[i + 1]
	denom <- dm1 - 2 * d0 + dp1
	if (denom == 0) return(M)
	M + 0.5 * (dm1 - dp1) / denom
}
