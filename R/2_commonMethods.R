#############################################################################
###
###         common methods
###


setGeneric("getOmega", function(object) standardGeneric("getOmega")) #Method to get the last attainable age

# For multiple decrement tables, omega is the largest age stored in table$x.
setMethod("getOmega","mdt", 
		function(object) {
			max(object@table$x)
		}
)


# For life tables, omega is the largest age stored in slot x.
setMethod("getOmega","lifetable", 
		function(object) {
			max(object@x)
		}
)
