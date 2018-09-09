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
###         common methods
###


setGeneric("getOmega", function(object) standardGeneric("getOmega")) #Method to get the last attainable age

setMethod("getOmega","mdt", 
		function(object) {
			out=numeric(1)
	#		out=max(object@x)+1
			out=max(object@table$x)
			return(out)}
)


setMethod("getOmega","lifetable", 
		function(object) {
			out=numeric(1)
#		out=max(object@x)+1
			out=max(object@x)
			return(out)}
)

setMethod("getOmega","actuarialtable", 
		function(object) {
			out=numeric(1)
			out=max(object@x)
			return(out)
		}
)