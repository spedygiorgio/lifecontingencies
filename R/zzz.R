# TODO: Add comment
# 
# Author: Giorgio
###############################################################################


# display version number and date when the package is loaded
.onAttach <- function(libname, pkgname) {
	desc  <- packageDescription(pkgname, libname)
	packageStartupMessage(
			'Version:  ', desc$Version, '\n', 
			'Date:     ', desc$Date, '\n',
			'Author:   ', 'Giorgio Alfredo Spedicato Ph.D C.Stat ACAS \n with significant contributions from Reinhold Kainhofer and Kevin J. Owens'
	)
}

# onload function: registering the vignette engine
# .onLoad <- function(libname, pkgname) {
#   tools::vignetteEngine("rmarkdown", weave = vweave, tangle = vtangle,
#                         pattern = "[.]Rmd$", package = "knitr")
# }