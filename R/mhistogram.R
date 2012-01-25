
mhistogram <- function(..., n=12, nint=n) {
	if ( require(manipulate) ) {
		manipulate(
			xhistogram( ..., nint = N, fit=FIT ),
			N = slider( 3, 50, initial=nint ),
			FIT = picker( none=NULL, 
						 normal="normal", 
						 "log-normal" = "log-normal",
						 exponential='exponential', 
						 gamma='gamma'
						 )
		)
	}
	else {
		histogram(..., nint=nint)
	}
}


