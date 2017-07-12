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

#' @useDynLib lifecontingencies
#' @importFrom Rcpp sourceCpp
NULL