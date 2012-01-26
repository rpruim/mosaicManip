
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


