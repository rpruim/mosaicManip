
.requireManipulate <- function() {
	if (! require(manipulate) ) {
		stop("Must run in a manipulate compatible system, e.g. RStudio")
	}
}
