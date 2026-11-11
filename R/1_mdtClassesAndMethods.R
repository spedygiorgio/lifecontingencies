#############################################################################
#   Copyright (c) 2018 Giorgio A. Spedicato
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the
#   Free Software Foundation, Inc.,
#   59 Temple Place, Suite 330, Boston, MA 02111-1307, USA
#
#############################################################################
###
###         multiple decrement
###

.MDT_DEFAULT_NAME <- "sample multiple decrement table"
.MDT_DEFAULT_TABLE <- data.frame(
	x = seq(0, 2, 1),
	lx = c(1000, 500, 200),
	c1 = c(200, 200, 100),
	c2 = c(300, 100, 100)
)
# Default survival ratio used to back-fill synthetic rows below the lowest
# age actually supplied (e.g. a table starting at age 50 gets ages 0-49
# reconstructed assuming this constant one-year survival). Kept as the
# default for backward compatibility; callers can override it via the
# `bottomCompletionSurvival` argument of new("mdt", ...) -- see
# .tableSanitizer() below.
.MDT_BOTTOM_COMPLETION_SURVIVAL <- 0.99

# Decrement totals are compared with lx up to a relative tolerance rather
# than exactly. A table whose decrements come from floating-point rates
# (buildMdtFromIndependentRates(), a graduated table, any non-integer input)
# balances only up to rounding, and the rounding itself is platform
# dependent: the same table that balanced to the last bit on x86 was rejected
# with "Check the lx" on aarch64-apple-darwin, where the compiler contracts
# a * b + c into a fused multiply-add. Integer-valued published tables are
# unaffected (they balance exactly, well inside the tolerance).
.MDT_BALANCE_TOL <- 1e-8

# TRUE when `total` equals `reference` up to .MDT_BALANCE_TOL, relatively for
# non-zero references and absolutely otherwise.
.mdtBalanced <- function(total, reference) {
  if (length(total) != 1L || length(reference) != 1L ||
      anyNA(c(total, reference)))
    return(FALSE)
  scale <- max(abs(total), abs(reference))
  if (scale == 0) return(TRUE)
  abs(total - reference) <= .MDT_BALANCE_TOL * scale
}

#defines the multiple decrement class
#at least three or more slots, x, lx and the causes...
setClass("mdt",
		slots = c(name = "character", table = "data.frame")
)
#defines the validity of a multiple decrement table
setValidity("mdt",
		function(object){
			# Collect all validation failures to ease debugging in validObject().
			check <- character(0)
			namesOfTable <- names(object@table)
			if(!all(c("x", "lx") %in% namesOfTable)) {
				check <- c(check, "Missing x or lx")
			}

			if(all(c("x", "lx") %in% namesOfTable)) {
				# Check that x is a consecutive sequence from 0 to max(x).
				if(!setequal(object@table$x, seq(0, max(object@table$x), by = 1))) {
					check <- c(check, "Check the x sequence")
				}

				# drop = FALSE preserves matrix/data.frame semantics with one decrement.
				# Check that the total decrements are equal to initial lx.
				onlyDecrements <- object@table[, setdiff(namesOfTable, c("x", "lx")), drop = FALSE]
				if(!.mdtBalanced(sum(onlyDecrements), object@table$lx[1])) {
					check <- c(check, "Check the lx")
				}
			}

			if(length(check) == 0) {
				TRUE
			} else {
				check
			}
		}
)

#initialize method

setMethod("initialize",
		signature(.Object = "mdt"),
		function (.Object,
		          name = .MDT_DEFAULT_NAME,
		          table = .MDT_DEFAULT_TABLE,
		          bottomCompletionSurvival = .MDT_BOTTOM_COMPLETION_SURVIVAL,
		          ...)
		{
			table<-.tableSanitizer(decrementDf=table, bottomCompletionSurvival=bottomCompletionSurvival)
			callNextMethod(.Object=.Object, name =name, table=table,...)
		}
)



#' Decrement names of an \code{mdt} object
#'
#' Return the names of the decrement (cause-of-exit) columns stored in a
#' \code{\linkS4class{mdt}} multiple-decrement table, i.e. every column in
#' the underlying data frame other than the structural \code{x} (age) and
#' \code{lx} (survivors) columns.
#'
#' @param object A \code{\linkS4class{mdt}} object.
#'
#' @return A character vector of decrement identifiers.
#' @references Finan, M.B. \emph{A Reading of the Theory of Life Contingency
#'   Models: A Preparation for Exam MLC/3L}.
#' @author Giorgio Alfredo Spedicato
#' @seealso \code{\link{getOmega}}, \code{\linkS4class{mdt}}
#' @examples
#' tableDecr <- data.frame(d1 = c(150, 160, 160), d2 = c(50, 75, 85))
#' newMdt <- new("mdt", name = "testMDT", table = tableDecr)
#' getDecrements(newMdt)
#' @export
setGeneric("getDecrements", function(object) standardGeneric("getDecrements"))

#' @rdname getDecrements
#' @export
setMethod("getDecrements","mdt",
		function(object) {
			# Everything that is not the age or the survivor count is a decrement.
			out <- setdiff(names(object@table), c("x","lx"))
			return(out)
		}
)

#function to check if a number is a probability

.isProb<-function(prob)
{
	if((prob > 1) || (prob < 0)) return(FALSE) else return(TRUE)
}

# Normalise a user-supplied decrement data frame into the full canonical
# layout expected by new("mdt", ...).
# The input is allowed to omit lx and/or x: lx is reconstructed from the
# decrement counts, missing lower ages are padded assuming the one-year
# survival ratio `bottomCompletionSurvival` (constant below the lowest age
# supplied), and a terminal row is appended if the decrements of the last
# age don't exhaust the survivors - all missing survivors are attributed
# to the first decrement, which is a convention, not a model.
.tableSanitizer<-function(decrementDf, bottomCompletionSurvival = .MDT_BOTTOM_COMPLETION_SURVIVAL)
{
	if (!.isProb(bottomCompletionSurvival))
		stop("Error! bottomCompletionSurvival must be a probability in [0,1]")
	out<-decrementDf
	namesOfTable<-names(decrementDf)
	decrementIds<-which(!(namesOfTable %in% c("lx","x")))
	pureDecrements<-decrementDf[,decrementIds, drop = FALSE]
	#add the lx column if the user supplied only decrement counts
	if(!("lx" %in% namesOfTable))
	{
		lx<-numeric(nrow(decrementDf))
		lx[1]<-sum(pureDecrements)
		for(i in 2:length(lx))
		{
			lx[i]=lx[i-1]-sum(pureDecrements[i-1,, drop = FALSE])
			
		}
		out$lx<-lx
		decrementDf<-out
		cat("Added lx","\n")
	}
	#add the x column
	if(!("x" %in% namesOfTable)) #check if missing x
	{
		x=seq(from=0,to=(nrow(decrementDf)-1),by=1)
		out$x<-x
		decrementDf<-out
		cat("Added x to the table...","\n")
	}
	#complete the table from bottom
	if(!(min(decrementDf$x)==0))
	{
		x2Complete<-seq(from=0,to=(min(decrementDf$x)-1))
		lx2Complete<-numeric(length(x2Complete))
		lxLast<-decrementDf$lx[1]
		for(i in rev(seq_along(lx2Complete)))
		{
			# Reconstruct previous lx values by applying the (possibly
			# user-supplied) synthetic survival ratio.
			lx2Complete[i]<-lxLast/bottomCompletionSurvival
			lxLast<-lx2Complete[i]
		}
		dx2Add<- -diff(c(lx2Complete,decrementDf$lx[1]))
		decrements2complete<-matrix(0,nrow=length(dx2Add),ncol=ncol(decrementDf),
				dimnames=list(NULL,c("x","lx",colnames(decrementDf)[decrementIds])))
		decrements2complete[,1]<-x2Complete #writing x
		decrements2complete[,2]<-lx2Complete #writing lx
		decrements2complete[,3]<-dx2Add #writing on the first decrement
		outMatrix<-rbind(decrements2complete,as.matrix(decrementDf))
		out<-as.data.frame(outMatrix)
		rownames(out)<-NULL
		cat("Added fictional decrement below last x and completed x and lx until zero....","\n")
	}
	#complete the table for top
	maxage<-which(out$x==max(out$x))
	pureDecrements<-out[,decrementIds, drop = FALSE]
	lastCheck<-.mdtBalanced(sum(pureDecrements[maxage,, drop = FALSE]), out$lx[maxage])
	if (!lastCheck) {
		# Add one terminal row so that all remaining lives are decremented.
		decrements2complete<-matrix(0,nrow=1,ncol=ncol(decrementDf),dimnames=list(NULL,c("x","lx",colnames(decrementDf)[decrementIds])))
		decrements2complete[1,1]<-max(out$x)+1
		decrements2complete[1,2]<-out$lx[1]-sum(out[,decrementIds, drop = FALSE])
		decrements2complete[1,3]<-decrements2complete[1,2]
		outMatrix<-rbind(out,decrements2complete)
		out<-as.data.frame(outMatrix)
		rownames(out)<-NULL
		cat("Completed the table at top, all decrements on first cause","\n")
	}
	invisible(out)
}

#function to shows probabilities instead of decrements
.decr2Probs<-function(decrementDf)
{
	namesOfTable<-names(decrementDf)
	decrementIds<-which(!(namesOfTable %in% c("lx","x")))
	pureDecrements<-decrementDf[,decrementIds, drop = FALSE]
	probs<-pureDecrements/decrementDf$lx
	rownames(probs)<-decrementDf$x
	invisible(probs)
}

#show method

setMethod("show","mdt", #metodo show
		function(object){
			cat(paste("Multiple decrements table",object@name),"\n")
			object@table
			print(object@table)
		}
)

setMethod("print","mdt", #metodo show
		function(x){
			cat(paste("Multiple decrements table",x@name),"\n")
			probs<-.decr2Probs(x@table)
			print(probs)
		}
)

#export method

#saves mdt as a data frame
setAs("mdt","data.frame",
		function(from){
			return(from@table)
		}
)


#summary method

setMethod("summary",
		signature(object="mdt"),
		function (object, ...)
		{
			cat("This is Multiple Decrements Table: ",object@name, "\n","Omega age is: ",getOmega(object), "\n", "Stored decrements are: ", getDecrements(object))
		}
)


#' Plot a multiple decrement table
#'
#' S4 method for the \code{\link[graphics]{plot}} generic: visualises the
#' decrement structure of an \code{mdt} object.  Three views are available:
#' a stacked-area chart of death counts \eqn{d^{(j)}_x} (default), a stacked
#' bar chart, or a line chart of decrement probabilities
#' \eqn{q^{(j)}_x = d^{(j)}_x / l^{(\tau)}_x}.
#'
#' @param x An \code{mdt} object.
#' @param y Not used (kept for S4 generic compatibility).
#' @param type Character: one of \code{"area"} (default), \code{"bar"}, or
#'   \code{"probability"}.  \code{"area"} and \code{"bar"} show stacked
#'   decrement counts; \code{"probability"} shows decrement-specific
#'   probabilities as lines.
#' @param ... Further arguments (currently unused).
#'
#' @return A \code{ggplot2} object (returned invisibly).
#'
#' @examples
#' valdezDf <- data.frame(
#'   x = 50:54,
#'   lx = c(4832555, 4821937, 4810206, 4797185, 4782737),
#'   heart = c(5168, 5363, 5618, 5929, 6277),
#'   accidents = c(1157, 1206, 1443, 1679, 2152),
#'   other = c(4293, 5162, 5960, 6840, 7631))
#' valdezMdt <- new("mdt", name = "ValdezExample", table = valdezDf)
#' plot(valdezMdt)
#' plot(valdezMdt, type = "bar")
#' plot(valdezMdt, type = "probability")
#'
#' @importFrom ggplot2 ggplot aes geom_area geom_col geom_line labs theme_minimal
#' @exportMethod plot
setMethod("plot", signature(x = "mdt", y = "missing"),
	function(x, y, type = c("area", "bar", "probability"), ...) {
		type <- match.arg(type)
		tbl <- x@table
		decrements <- getDecrements(x)

		# Reviewer note: .tableSanitizer() appends a synthetic terminal row to
		# "close" the table whose lx carries the remaining survivors and whose
		# decrement cells are copies of that same number on the first cause.
		# Plotting it would double-count the last real age, so we drop it here.
		ages <- tbl$x[-nrow(tbl)]

		# Reshape to one row per (age, decrement) so ggplot2 can colour / stack
		# by `decrement`. unlist(lapply(...)) traverses the decrements in the
		# same order used by rep(..., each = length(ages)) above, so column
		# alignment holds without an explicit join.
		longDf <- data.frame(
			age = rep(ages, times = length(decrements)),
			decrement = rep(decrements, each = length(ages)),
			value = unlist(lapply(decrements, function(d) tbl[[d]][-nrow(tbl)])),
			stringsAsFactors = FALSE
		)
		# Keeping the factor levels in the table's own column order makes the
		# stacking (and the legend) deterministic across themes/ggplot2 versions.
		longDf$decrement <- factor(longDf$decrement, levels = decrements)

		if (type == "probability") {
			# Divide by lx to get decrement-specific probabilities
			lxVec <- tbl$lx[-nrow(tbl)]
			longDf$value <- longDf$value / rep(lxVec, times = length(decrements))
			p <- ggplot2::ggplot(longDf, ggplot2::aes(x = age, y = value,
			                                           colour = decrement)) +
				ggplot2::geom_line(linewidth = 0.8) +
				ggplot2::labs(
					title = paste0("Decrement probabilities: ", x@name),
					x = "Age", y = expression(q[x]^{(j)}),
					colour = "Decrement"
				) +
				ggplot2::theme_minimal()
		} else if (type == "bar") {
			p <- ggplot2::ggplot(longDf, ggplot2::aes(x = age, y = value,
			                                           fill = decrement)) +
				ggplot2::geom_col() +
				ggplot2::labs(
					title = paste0("Decrement counts: ", x@name),
					x = "Age", y = expression(d[x]^{(j)}),
					fill = "Decrement"
				) +
				ggplot2::theme_minimal()
		} else {
			# stacked area (default)
			p <- ggplot2::ggplot(longDf, ggplot2::aes(x = age, y = value,
			                                           fill = decrement)) +
				ggplot2::geom_area() +
				ggplot2::labs(
					title = paste0("Decrement counts: ", x@name),
					x = "Age", y = expression(d[x]^{(j)}),
					fill = "Decrement"
				) +
				ggplot2::theme_minimal()
		}
		invisible(p)
	}
)