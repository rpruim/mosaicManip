#' Manipulate histograms
#' 
#' Histograms created with \code{\link{xhistogram}} are augmented with
#' additional controls that allow the user to adjust bin widths and centers
#' and to overlay various density curves
#' 
#' @param \dots additional arguments passed to \code{\link{xhistogram}}
#' @param center initial center of one of the bins.  A slider is set up 
#' 		with this value as its initial value.
#' @param width initial width of the bins.  A slider is set up 
#' 		with this value as its initial value.
#' @return \code{mhistogram} is called primarily for the interactive
#' 	histogram that it creates.
#' 
#' @details
#' If the \code{manipulate} package is unavailable, 
#' then \code{\link{xhistogram}} 
#' is used to generate a static histogram.
#' 
#' @seealso \code{\link{xhistogram}}
#' 
#' @examples
#' mhistogram( ~ eruptions, data=faithful, center=3, width=0.3)
#' mhistogram( ~ age | substance, data=HELPrct, center=45, width=5)

mhistogram <- function(..., center=1, width=1) {
	if ( require(manipulate) ) {
		manipulate(
			xhistogram( ..., fit=FIT, center=CENTER, width=WIDTH ),
			CENTER = slider( 0, 5*center, initial=center, step=center/20 ),
      WIDTH = slider(width/20, 10*width, initial=width, step=width/20),
			FIT = picker( none=NULL, 
						 normal="normal", 
						 "log-normal" = "log-normal",
						 exponential='exponential', 
						 gamma='gamma'
						 )
		)
	}
	else {
		xhistogram(..., center=center, width=width)
	}
}


