
mInfluence <- function(expr, data, 
					   groups=NULL, 
					   col.fitted = 'gray30', 
					   col.influence='red', ...){
    if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
	dots <- list(...)
	groups <- eval(substitute(groups), data, environment(expr))
	if (!require(manipulate) | !require(lattice) | !require(grid)) stop("Must have manipulate package.")
	xvar  <-  list();
	yvar  <-  all.vars(expr)[1]

	terms <- terms(expr)
	nterms <- length(terms)
	for(a in all.vars(expr)[-1]){  # Name the levels of xvar, filled with char string
		xvar[[a]] <- a
	}
	#====================
	myFun <- function(xpick, multX, multY, newPoint, groups=NULL, ...){
		xdat <- data[[xvar[[xpick]]]]
		ydat <- data[[yvar]]
		newRow <- data[1,]
		for(b in 1:length(newRow)){
			if(is.numeric(newRow[1,b]))     # At the moment just leaving the categorical variables as is
				newRow[1,b] <- median(data[,b])  # That is, what they were from data[1,], first row of data
		}

		if(is.numeric(newRow[[xpick]])){
			newRow[[1,xpick]] <- multX*sd(xdat)+mean(xdat) # This needs to be adjusted to allow categorical 
		}                                                   # variables to work. mean(factor) doesn't work
		if(is.factor(newRow[[xpick]])){
			newRow[[1,xpick]] <- levels(xdat)[floor(length(levels(xdat))*(multX+5)/10.001)+1]
		}
		if(is.numeric(newRow[[1,yvar]])){
			newRow[[1,yvar]] <- multY*sd(ydat)+mean(ydat)
		}
		if(is.numeric(xdat))
			maxxlim <- c(-5.2*sd(xdat)+mean(xdat), 5.2*sd(xdat)+mean(xdat))  # manipulate control params
		maxylim <- c(-5.2*sd(ydat)+mean(ydat), 5.2*sd(ydat)+mean(ydat))
		modData <- rbind(data, newRow)
		if(newPoint)
			data <- modData
		if(is.factor(data[[xvar[[xpick]]]])){
			xlevels <- levels(xdat)
		}
		mod <- lm(expr, data)
		influ <- influence.measures(mod)
		influIdx <- which(apply(influ$is.inf, 1, any))
		#     influIdx <- data[influIdx,]

		panel.mInfluence <- function(x,y,groups=NULL,...){
			if(is.factor(x)) {
				set.seed(73241)
				x <- jitter(as.numeric(x))
			}

				panel.xyplot(x,y, group=groups, ...)  # Data
				# overplot Influential Data 
				mosaic:::.do.safe.call( panel.xyplot, groups=groups[influIdx],
							  list(x=x[influIdx], y=y[influIdx], col=col.influence,
								   cex= if(is.null(dots[['cex']])) .6 else .6*dots[['cex']]), 
							  ... )  
				# Add fitted data points
				mosaic:::.do.safe.call(panel.xyplot, list(x=x, y=fitted(mod), pch=18, col=col.fitted), ... )  
				if(newPoint){
					mosaic:::.do.safe.call( panel.xyplot, 
								  list(x=x[length(x)], y=y[length(y)], col="orange", lwd=3,
									   cex= if(is.null(dots[['cex']])) 1.3 else 1.3*dots[['cex']]), 
								  ...)  # last point is newpoint
				}
			}

			if(is.factor(xdat)){
				xyplot( data[[yvar]] ~ data[[xvar[[xpick]]]], 
					   ylab=yvar, 
					   xlab=xvar[[xpick]], 
					   groups=groups,
					   panel=panel.mInfluence, 
					   #panel.groups=panel.mInfluence, 
					   ylim=maxylim,
					   scales=list(x=list(labels=xlevels,at=seq(1,length(xlevels),by=1))), 
					   ...)
			}
			else
				mosaic:::.do.safe.call( lattice:::xyplot.formula, list( x = data[[yvar]] ~ data[[xvar[[xpick]]]], 
					   ylab=yvar, 
					   xlab=xvar[[xpick]],
					   groups=groups,
					   panel=panel.mInfluence, 
					   #panel.groups="panel.mInfluence", 
					   xlim=maxxlim, 
					   ylim=maxylim) , 
					   ...)
	}
  #==================
  controls <- list(xpick=picker(xvar, label="Predictor to plot"),
                newPoint=checkbox(FALSE, label="Add new point"),
                multX=slider(-5, 5, initial=1, label="New predictor (measured in SDs)"),
                multY=slider(-5, 5, initial=1, label="New response (measured in SDs)")
            ##    influPick=picker(cooks.distance="Cook's Distance", label="Type of Influence")
            ## Influence picker never implemented, Influence calculated by
            ## influence.measures is.inf    
                )
  manipulate(myFun(xpick=xpick, multY=multY, multX=multX, newPoint=newPoint, groups=groups, ...), 
             controls)
}
