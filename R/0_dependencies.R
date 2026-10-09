# markovchain is an optional dependency (Suggests). Functions that need it call
# this first and stop with a clear message if it is not installed; the S4
# coercions to "markovchainList" are registered only when it is available
# (see .onLoad() in zzz.R).
.markovchainAvailable <- function() requireNamespace("markovchain", quietly = TRUE)

.require_markovchain <- function(function_name) {
	if (!.markovchainAvailable()) {
		stop(
			"`", function_name, "` requires the optional 'markovchain' package. ",
			"Install it with install.packages(\"markovchain\").",
			call. = FALSE
		)
	}
	invisible(TRUE)
}
