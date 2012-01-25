#' Interactive line-fitting application
#' 
#' An interactive demonstration program for showing how the residuals
#' and the sum of square residuals relates to the slope and intercept of a fitted
#' line.  There are sliders to modify the slope and intercept of the fitted
#' line and checkboxes that control whether to display the residuals (as lines)
#' or the sum of square residuals (as filled boxes).  The numerical value
#' of the sum of square residuals is always shown.  By modifying the slope and intercept,
#' you can see how the sum of square residuals increases compared to the 
#' optimal, least squares value.
#' 
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#'
#' @param form a model formula with a single, quantitative explanatory variable
#' @param data  data frame containing the data for fitting
#'
#' @return nothing
#' @examples
#' 
#' \dontrun{
#' kids = fetchData("kidsfeet.csv")
#' mLineFit( length ~ width, data=kids )
#' }

mLineFit <- function(form, data){
  # define a helper function internally
.mLineFitHelper <- function(formula, data=NULL,...){
	if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
	mod <- lm( formula, data=data, ... )
	responseName <- deparse(mod$terms[[2L]])
	bestSlope <- coef(mod)[2]
	bestInter <- coef(mod)[1]
	mat <- model.matrix(mod)
	independIndex <- min(ncol(mat), 2)
	response <- fitted(mod) + resid(mod)
	explanatory <- mat[,independIndex]
	vecnames <- colnames(mat)
	mx <- mean(explanatory)
	my <- mean(response)

	showResids <- FALSE
	showSquares <- FALSE
	interceptRange <- sort(sd(response)*c(-2,2))
	curIntercept <- mean(interceptRange)
	slopeRange <- sort(coef(mod)[2]*c(-1,1.5))
	curSlope <- 1;

	showPlot <- function(curSlope,curIntercept,showResids,showSquares){
		modelVals <- curSlope*(explanatory-mx) + (curIntercept+my)
		plot( explanatory, response,
			 ylab=responseName,
			 xlab=vecnames[independIndex],
			 pch=20,col="blue")

		if( showResids ) {
			for( k in 1:length(explanatory) ) {
				thisColor <- c("red","blue","blue")[sign(response[k]-modelVals[k])+2]
				lines( c(0,0)+explanatory[k], c(response[k],modelVals[k]), col=thisColor)
			}
		}
		if( showSquares )  {
			foo <- par("usr")
			goo <- par("pin")
			text(foo[1],foo[4]-.05*(foo[4]-foo[3]),paste("Sum Sq. Resids=",signif(sum((response-modelVals)^2),3)),pos=4)
			hscale <- (goo[2]/goo[1])*(foo[2]-foo[1])/(foo[4]-foo[3]) # scale horizontal appropriately
			for( k in 1:length(explanatory) ) {
				hlength <- hscale*abs(response[k] - modelVals[k])
				polygon( explanatory[k]+c(0,0,hlength,hlength),
						c(response[k],modelVals[k],modelVals[k],response[k]),
						col=rgb(1,0,0,.1), border=NA)
			}
		} 

		abline(curIntercept+my-curSlope*mx, curSlope,col=rgb(0,0,0,.3),lwd=2)
		points( explanatory, modelVals, pch=10, col="black") 
		points( explanatory, response, pch=20, col="blue")


	}

	doPlot <- function(slope,inter,resids,squares) {
		curSlope  <-  slope*bestSlope
		curIntercept  <-  inter
		showResids <-  resids
		showSquares <-  squares
		showPlot(curSlope,curIntercept,showResids,showSquares)
	}
	return(doPlot)
}

# ==================
# The manipulate action

	f <- .mLineFitHelper( form, data)
	manipulate( f(slope,inter,resids,squares), 
			   inter = slider(-3,3,initial=0,step=.05,ticks=FALSE,label="Intercept Offset"),
			   slope = slider(-1,2,initial=1,step=.05,ticks=FALSE,label="Slope Multiplier"),
			   resids = checkbox(FALSE,label="Show Residuals"),
			   squares = checkbox(FALSE,label="Square resids"))
}
