#' Manipulate histograms
#' 
#' Histograms created with \code{\link{xhistogram}} are augmented with
#' additional controls that allow the user to adjust bin widths and centers
#' and to overlay various density curves
#' 
#' @param \dots additional arguments passed to \code{\link{xhistogram}}
#' @param width initial width of the bins.  A slider is set up 
#' 		with this value as its initial value.
#' @param center ignored if \code{manipulate} is available, in which
#'      a slider is generated based on the value of \code{width}.  
#'      Otherwise passed through to \code{\link{xhistogram}}.
#' @param fit ignored if \code{manipulate} is available, in which
#'      a menu of fit options is available.  
#'      Otherwise passed through to \code{\link{xhistogram}}.
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

mhistogram <- function(..., width=1, center=NULL, fit=FALSE) {
	if ( require(manipulate) ) {
		manipulate(
			xhistogram( ..., fit=FIT, center=CENTER, width=WIDTH ),
			CENTER = slider( 0, width, initial=0, step=width/40 ),
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
		xhistogram(..., center=center, width=width, fit=fit)
	}
}


