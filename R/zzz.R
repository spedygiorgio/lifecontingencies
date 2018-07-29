# TODO: Add comment
# 
# Author: Giorgio
###############################################################################



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