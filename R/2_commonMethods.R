# TODO: Add comment
# 
# Author: Giorgio
###############################################################################


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