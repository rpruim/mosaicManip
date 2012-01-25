mTaylor <- function(expr, xlim = c(-5, 5), ...){
	# packages
    if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
	# functions
	vals <- list(...)
	fm <- mosaic:::.createMathFun( sexpr=substitute(expr), ...)
	f <- fm$fun
	x <- seq(min(xlim), max(xlim), length = 1000)
	# colors
	trans.blue <- rgb(0,0,1,.1) # least squares rectangle color
	rect.trans.blue <- rgb(0,0,1,.05)
	trans.red <- rgb(1,0,0,.1) # red polygon fill color
	font.size <- 13  

	line.blue <- rgb(0,0,1,.5) # least squares line color
	line.red <- rgb(1,0,0,.5) # Taylor line color
	# Derivatives of function for Taylor Series
	dd <- list()
	dd[[1]] <- mosaic:::.d.symbolic(fm)
	var <- fm$RHS
	for(k in 2:10){
		fm$RHS <- c("+", var, fm$RHS)
		dd[[k]] <- mosaic:::.d.symbolic(fm)
	}

	myplot <- function(a, TaylorBeTrue, lsquares, xwid, n, err, which.plot){
		# Taylor Series
		T <- list()
		# zeroth order is index 1
		T[[1]] <- f(a) + 0*x
		for (k in 2:11) {
			T[[k]] <- T[[k-1]] + (dd[[k-1]](a)*((x-a)^(k-1)))/factorial(k-1)
		}

		# Least Squares: Row 1 is order 0
		myx <- seq(a-xwid/2, a+xwid/2, length = 1000)
		A <- outer(myx-a, 0:n, "^")
		coefs <- qr.solve(A, f(myx))
		lsq.func <- 0
		for(j in 0:n){
			lsq.func <- lsq.func+(x-a)^(j)*coefs[[j+1]]
		}

		# plotting
		mypanel <- function(x, y){
			panel.points(a, f(a), cex = 2)
			if(lsquares){
				panel.xyplot(x, lsq.func, type = "l", col = line.blue, lwd = 5)
				panel.rect(xright=min(myx), 
						   xleft=max(myx), 
						   ybottom=-999999, 
						   ytop = 999999, 
						   col = rect.trans.blue,
						   border = FALSE) 
				ypts <- lsq.func
				# lSquares RMSE from myPanel
				inds <- which( x > min(myx) & x < max(myx) )
				.newY <- c(0,y,0); .newYPts <- c(0,ypts,0)
				myY <- .newYPts[inds]-.newY[inds]
				lsRMS <- abs(sqrt(mean(myY^2))*diff(range(myx)))
				grid.text(label = paste("Least Squares RMS error: ", signif(lsRMS,4)), 
						  x = unit(0,"npc")+unit(1, "mm"), 
						  y = unit(1,"npc")-unit(1.5, "lines"),
						  just ="left",
						  gp = gpar(col = "blue", fontsize =font.size)) 
			}

			if(TaylorBeTrue){
				panel.points(x, T[[as.numeric(n)+1]], type = "l", col = line.red, lwd = 5)

				ypts <- c(T[[as.numeric(n)+1]])
				# TAYLOR RMSE from myPanel
				inds <- which(x>min(myx)&x<max(myx))
				.newY <- c(0,y,0); .newYPts <- c(0,ypts,0);
				myY<-.newYPts[inds]-.newY[inds]
				TayRMS <- abs(sqrt(mean(myY^2))*diff(range(myx)))
				grid.text(label = paste("Taylor Series RMS error: ", signif(TayRMS,4)), 
						  x = unit(0,"npc")+unit(1, "mm"), 
						  y = unit(1,"npc")-unit(.5, "lines"),
						  just ="left",
						  gp = gpar(col = "red", fontsize =font.size))
			}

			if(err == TRUE){  
				xpts <- c(x)
				if( lsquares ){
					ypos <- pmax(f(x), ypts)
					yneg <- pmin(f(x), ypts)
					panel.polygon(c(xpts,rev(xpts)),c(ypos,rev(yneg)),col=trans.blue)
				}

				if(TaylorBeTrue){
					ypos <- pmax(f(x), ypts)
					yneg <- pmin(f(x), ypts)
					panel.polygon(c(xpts,rev(xpts)),c(ypos,rev(yneg)),col=trans.red)
				}
			}  
			panel.xyplot(x, y, type = "l", col = "black", lwd = 2)
		}
		# ============
		errpanel <- function(x,y){
			panel.ylab=
			xpts = c(min(x),x,max(x))
			if(TaylorBeTrue)
			{
				ypts <- c(0,T[[as.numeric(n)+1]]-y,0)
				# TAYLOR RMSE from errPanel
				inds <- which(x>min(myx)&x<max(myx))
				myY <- ypts[inds]
				TayRMS <- abs(sqrt(mean(myY^2))*diff(range(myx)))
				grid.text(label = paste("Taylor Series RMS error: ", signif(TayRMS,4)), 
						  x = unit(0,"npc")+unit(1, "mm"), 
						  y = unit(1,"npc")-unit(.5, "lines"),
						  just ="left",
						  gp = gpar(col = "red", fontsize =font.size))

				panel.xyplot(x, (T[[as.numeric(n)+1]]-y), type = "l", col = line.red, lwd = 5)
				if(err == TRUE)
				{
					ypos <- pmax(0, ypts)
					yneg <- pmin(0, ypts)
					panel.polygon(xpts,ypos,col=trans.red, border = FALSE)
					panel.polygon(xpts,yneg,col=trans.red, border = FALSE) 
				}
			}
			if(lsquares){
				panel.xyplot(x, (lsq.func-y), type = "l", col = line.blue, lwd = 5)
				panel.rect(xright=min(myx), 
						   xleft=max(myx), 
						   ybottom=-999999, 
						   ytop = 999999, 
						   col = rect.trans.blue,
						   border = FALSE)
				ypts <- c(0,lsq.func-y,0)           
				# LSquares RMSE from errPlot
				inds <- which(x>min(myx)&x<max(myx))
				myY <- ypts[inds]
				lsRMS <- abs(sqrt(mean(myY^2))*diff(range(myx)))
				grid.text(label = paste("Least Squares RMS error: ", signif(lsRMS,4)), 
						  x = unit(0,"npc")+unit(1, "mm"), 
						  y = unit(1,"npc")-unit(1.5, "lines"),
						  just ="left",
						  gp = gpar(col = "blue", fontsize =font.size))
				if(err == TRUE){
					ypos <- pmax(0, ypts)
					yneg <- pmin(0, ypts)
					panel.polygon(xpts,ypos,col=trans.blue, border = FALSE)
					panel.polygon(xpts,yneg,col=trans.blue, border = FALSE)
				}
			}
			panel.points(a, 0, cex = 2)
			panel.abline(h=0, lty = "dotted")

		}
		x <- seq(min(xlim),max(xlim), length = 1000)

		if(which.plot =="Plot Functions"){
			xyplot(f(x)~x, xlim = c(min(x), max(x)), panel = mypanel)
		}
		else{
			xyplot(f(x)~x, xlim = c(min(x), max(x)), panel = errpanel, ylab="Error")
		}



	}
	# manipulate that!
	manipulate(myplot(a, TaylorBeTrue, lsquares, xwid, n, err, which.plot),
			   which.plot = picker("Plot Functions", "Plot Errors", label = "What to plot:"),
			   a = slider(min(xlim),max(xlim), initial = mean(xlim), 
						  step = diff(xlim)/100, label = "Position"),
			   n = picker(0,1,2,3,4,5,6,7,8,9,10, label = "Order of the Polynomial"),
			   TaylorBeTrue = checkbox(FALSE, "Activate Taylor Series"),
			   lsquares = checkbox(FALSE, "Activate least squares"),
			   xwid = slider(0, diff(range(xlim)), initial = mean(diff(range(xlim)))/2, 
							 step = diff(xlim/100), label = "Least Squares width"),
			   err = checkbox(FALSE, "View error area")

			   )  
}
