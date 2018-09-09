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
###         attach/unload functions
###


# TODO: Add comment
# 



# loading the markovchain package

.onAttach <- function(libname, pkgname) {
  desc  <- packageDescription(pkgname, libname)
  packageStartupMessage('Package:  ', desc$Package, '\n',
                        'Authors:  ', desc$Author, '\n',
                        'Version:  ', desc$Version, '\n', 
                        'Date:     ', desc$Date, '\n',
                        'BugReport: ', desc$BugReports, '\n')
}

# for unloading dynamic libraries

.onUnload <- function (libpath) {
  library.dynam.unload("lifecontingencies", libpath)
}

# onload function: registering the vignette engine
# .onLoad <- function(libname, pkgname) {
#   tools::vignetteEngine("rmarkdown", weave = vweave, tangle = vtangle,
#                         pattern = "[.]Rmd$", package = "knitr")
# }

#' @useDynLib lifecontingencies, .registration = TRUE 
#' @import methods parallel utils
#' @importClassesFrom markovchain markovchain markovchainList
#' @importFrom markovchain rmarkovchain
#' @importFrom stats rbinom complete.cases integrate
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @exportClass actuarialtable
#' @exportClass lifetable
#' @export accumulatedValue AExn annuity axn Axn Axn.mdt axyn Axyn axyzn Axyzn convertible2Effective convexity DAxn decreasingAnnuity 
#' @export discount2Interest duration dxt effective2Convertible exn Exn exyt exyzt getDecrements getLifecontingencyPv getLifecontingencyPvXyz 
#' @export getOmega head Iaxn IAxn increasingAnnuity intensity2Interest interest2Discount interest2Intensity Isn Lxt 
#' @export mx2qx mxt nominal2Real plot presentValue print probs2lifetable pxt pxyt pxyzt qxt.prime.fromMdt qxt.fromQxprime 
#' @export qx2mx qxt qxyt qxyzt real2Nominal rLife rLifeContingencies rLifeContingenciesXyz rLifexyz rmdt summary tail Tx 
#' @exportMethod coerce plot print show summary getOmega
NULL