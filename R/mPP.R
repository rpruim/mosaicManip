# Phase-Plane Software
# revise should be a push-button
mPP <- function( DE=predator.prey, xlim=c(-10,2000),ylim=c(-10,2000)) {
	if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
	on.exit()
	# Storage for the trajectories.  Starts out empty
	Tcolors <- c("red","cornflowerblue", "darkolivegreen3","gold","magenta")
	TcolorsBack <- c("deeppink","blue","darkolivegreen","gold3","magenta4")
	TS <- list()
	for (k in 1:length(Tcolors)) 
		TS[[k]] <- list(foward=NULL, back=NULL, system=DE, init=NULL)
	TStemp <- TS[[1]]
	# An initial condition
	initCond <- c(mean(xlim),mean(ylim))
	stateNames <- names(formals(DE))
	names(initCond) <- stateNames # needed so that solveDE will work
	reviseWhatState<-"initializing"
	storeWhatState<-"Working"
	# ========
	plotTraj <- function(soln, n=1001, ...) {
		t <- seq(soln$tlim[1], soln$tlim[2], length=n )
		one <- soln[[1]](t)
		two <- soln[[2]](t)
		llines(one, two, ...)
	}
	#========
	plotPort <- function(data, names, Ntraj, notNull, ...){
		xportPanel <- function(x,y,...){

			for(k in notNull){

				if(k==Ntraj){
					panel.xyplot(data[[paste("tf",k,sep="")]], data[[paste("xf",k,sep="")]], type = "l", col=Tcolors[[k]], lwd=2)
					panel.xyplot(data[[paste("tb",k,sep="")]], data[[paste("xb",k,sep="")]], type = "l", col=TcolorsBack[[k]], lwd=2)
				}
				else{
					panel.xyplot(data[[paste("tf",k,sep="")]], data[[paste("xf",k,sep="")]], type = "l", col=Tcolors[[k]])
					panel.xyplot(data[[paste("tb",k,sep="")]], data[[paste("xb",k,sep="")]], type = "l", col=TcolorsBack[[k]])
				}
			}
		}

		yportPanel <- function(x,y,...){
			for(j in notNull){
				if(j==Ntraj){
					panel.xyplot(data[[paste("tf",j,sep="")]], data[[paste("yf",j,sep="")]], type = "l", col=Tcolors[[j]], lwd=2)
					panel.xyplot(data[[paste("tb",j,sep="")]], data[[paste("yb",j,sep="")]], type = "l", col=TcolorsBack[[j]], lwd=2)
				}
				else{
					panel.xyplot(data[[paste("tf",j,sep="")]], data[[paste("yf",j,sep="")]], type = "l", col=Tcolors[[j]])
					panel.xyplot(data[[paste("tb",j,sep="")]], data[[paste("yb",j,sep="")]], type = "l", col=TcolorsBack[[j]])  
				}
			}
		}
		xmin<-Inf; xmax<-0; ymin<-Inf; ymax<-0; tmin<-0; tmax<-0;

		for(g in notNull){  
			##REDO with different data accessing? data[[x1]] currently not going through. Weird. Try calling data[[2]] for t1? Talk to DTK
			xmin <- min(data[[paste("xf",g,sep="")]], 
						data[[paste("xb",g,sep="")]], xmin, na.rm=TRUE)
			xmax <- max(data[[paste("xf",g,sep="")]],
						data[[paste("xb",g,sep="")]], xmax, na.rm=TRUE)
			ymin <- min(data[[paste("yf",g,sep="")]],
						data[[paste("yb",g,sep="")]], ymin, na.rm=TRUE)
			ymax <- max(data[[paste("yf",g,sep="")]],
						data[[paste("yb",g,sep="")]], ymax, na.rm=TRUE)
			tmin <- min(data[[paste("tf",g,sep="")]],
						data[[paste("tb",g,sep="")]], tmin, na.rm=TRUE)
			tmax <- max(data[[paste("tf",g,sep="")]],
						data[[paste("tb",g,sep="")]], tmax, na.rm=TRUE)      
		}
		xlims<-c(xmin, xmax)
		ylims<-c(ymin, ymax)
		tlims<-c(tmin, tmax)

		xport<-xyplot(xlims~tlims, panel=xportPanel, ylab=names[1], xlab=NULL, type = "l", lwd=3, scales=list(x=list(draw=FALSE)))
		yport<-xyplot(ylims~tlims, panel=yportPanel, ylab=names[2], xlab="t", type = "l", lwd=3)
		return(list(xport,yport))

	}
	#=========
	flowPlot <- function(fun,xlim=c(0,1), ylim=c(0,1), resol=10, col="black",
						 add=FALSE,EW=NULL,NS=NULL,both=TRUE) {
		current.dyn.system <<- fun
		arg.names <- names(formals(fun) )
		if (length( arg.names ) != 2 )
			stop("Must give dynamical function with two arguments.")
		if (add) {
			hoo <- current.panel.limits()
			xlim <- hoo$xlim
			ylim <- hoo$ylim
		}
		else{
			#panel.xyplot(x=0, y=0, xlim=xlim, ylim=ylim,
			#   xlab=arg.names[1], ylab=arg.names[2] )
		}

		x <- matrix(seq(xlim[1],xlim[2], length=resol), byrow=TRUE, resol,resol);
		y <- matrix(seq(ylim[1],ylim[2], length=resol),byrow=FALSE, resol, resol);
		npts <- resol*resol;
		xspace <- abs(diff(xlim))/(resol*5);
		yspace <- abs(diff(ylim))/(resol*5);
		set.seed(10101)
		x <- x + matrix(runif(npts, -xspace, xspace),resol,resol);
		y <- y + matrix(runif(npts, -yspace, yspace),resol,resol);
		z <- fun(x,y);
		z1 <- matrix(z[1:npts], resol, resol);
		z2 <- matrix(z[(npts+1):(2*npts)], resol, resol);
		maxx <- max(abs(z1));
		maxy <- max(abs(z2));
		dt <- min( abs(diff(xlim))/maxx, abs(diff(ylim))/maxy)/resol;
		lens <- sqrt(z1^2 + z2^2);
		lens2 <- lens/max(lens); 
		if( both ){
			larrows(c(x), c(y),
					c(x+dt*z1/((lens2)+.1)), c(y+dt*z2/((lens2)+.1)),
					length=.04, col=col);
		}
		if( !is.null(NS) ) {
			larrows(c(x), c(y),
					c(x), c(y+dt*z2/((lens2)+.1)),
					length=.04, col=NS);
		}
		if( !is.null(EW) ){
			larrows(c(x), c(y),
					c(x+dt*z1/((lens2)+.1)), c(y),
					length=.04, col=EW);
		}
	}
	#================
	jacobian <- function(fun=NULL,x=NULL, y=NULL,h=0.000001){
		if (is.null(fun) )  fun = current.dyn.system
		foo <- fun(x,y);
		foox <- fun(x+h,y);
		fooy <- fun(x,y+h);
		A <- (foox[1] - foo[1])/h;
		B <- (fooy[1] - foo[1])/h;
		C <- (foox[2] - foo[2])/h;
		D <- (fooy[2] - foo[2])/h;
		return(matrix( c(A,B,C,D ), 2,2, byrow=T))
	}

	# ========
	doPlot = function(xstart,ystart,Ntraj,tdur,tback,
					  nullclines=FALSE,reviseWhat,flowWhat,param1,param2,doJacob) {
		# set initial condition
		initCond[1] <<- xstart
		initCond[2] <<- ystart
		arg.names = names(formals(DE) )
		# Handle editing of the system, setting initial condition here
		# Need to set state manually to avoid lockup
		if( reviseWhatState != reviseWhat ) {
			# state changed, so do something
			reviseWhatState <<- reviseWhat
			#       if(!is.null(TS[[Ntraj]]$system)){
			#         DE <<- TS[[Ntraj]]$system
			#       }
			if( reviseWhat >= 0) {
				if(reviseWhat ==0){
					DE <<- edit(DE,title="Editing ALL dynamical systems")
					for(k in 1:5) TS[[k]]$system <<- DE
				}
				else
					TS[[reviseWhat]]$system <<- edit(TS[[reviseWhat]]$system,title=paste("Editing System",reviseWhat)) 
			}

		}
		# ... system editing code here

		# Store the results in the currently selected trajectory in "scratch" index 1
		TStemp$init <<- initCond
		TStemp$system <<- TS[[Ntraj]]$system
		# Find the forward trajectory
		if( tdur > 0 )
			TStemp$forward <<- solveDE( TStemp$system, init=initCond, tlim=c(0,tdur) )
		else TStemp$forward <<- NULL
		# Solve the trajectory backward here.  (Does solveDE do this?  Add a backward flag!)
		if (tback < 0 )
			TStemp$back <<- solveDE( TStemp$system, init=initCond, tlim=c(0,tback) )
		else TStemp$back <<- NULL

		TS[[Ntraj]]$init <<- TStemp$init
		TS[[Ntraj]]$system <<- TStemp$system
		TS[[Ntraj]]$forward <<- TStemp$forward
		TS[[Ntraj]]$back <<- TStemp$back

		notNull=NULL
		for(m in 1:5){
			if(!is.null(TS[[m]]$system))
				notNull=c(notNull, m)
		}
		TSfull=data.frame(index=seq(1,1000, length=1000))
		for(k in notNull){
			if(!is.null(TS[[k]]$forward)){
				TSfull[[paste("tf",k, sep = "")]] = seq(TS[[k]]$forward$tlim[1], TS[[k]]$forward$tlim[2], length=1000)
				TSfull[[paste("xf",k, sep = "")]] = TS[[k]]$forward[[1]](TSfull[[paste("tf",k, sep = "")]])
				TSfull[[paste("yf",k, sep = "")]] = TS[[k]]$forward[[2]](TSfull[[paste("tf",k, sep = "")]])
			}
		}

		for(k in notNull){
			if(!is.null(TS[[k]]$back)){
				TSfull[[paste("tb",k, sep = "")]] = seq(TS[[k]]$back$tlim[1], TS[[k]]$back$tlim[2], length=1000)
				TSfull[[paste("xb",k, sep = "")]] = TS[[k]]$back[[1]](TSfull[[paste("tb",k, sep = "")]])
				TSfull[[paste("yb",k, sep = "")]] = TS[[k]]$back[[2]](TSfull[[paste("tb",k, sep = "")]])
			}
		}
		#JACOBIAN
		.tmax=max(TS[[Ntraj]]$forward$tlim*.99999, 0, rm.na=TRUE)
		.tmin=min(TS[[Ntraj]]$back$tlim*.99999, 0, rm.na=TRUE)
		if(doJacob>0){
			if(doJacob==1) 
				jake <- jacobian(fun=TS[[Ntraj]]$dynfun, x=xstart, y=ystart)
			if(doJacob==3){
				jake <- jacobian(fun=TS[[Ntraj]]$dynfun, x=TS[[Ntraj]]$forward[[1]](.tmax), y=TS[[Ntraj]]$forward[[2]](.tmax))
			}
			if(doJacob==2)
				jake <- jacobian(fun=TS[[Ntraj]]$dynfun, x=TS[[Ntraj]]$back[[1]](.tmin), y=TS[[Ntraj]]$back[[2]](.tmin))
			eig=eigen(jake)
			print("Jacobian Matrix")
			print(jake)
			print("Eigenvalues")
			print(eig[1])
		}
		#Portrait Plots  
		port<-plotPort(TSfull, names=stateNames, notNull=notNull, Ntraj=Ntraj)


		#=============
		myPanel<-function(x,y, ...){
			# Plot out the flow field
			flowPlot( TS[[flowWhat]]$system, xlim=xlim, ylim=ylim)
			# Plot out the nullclines
			if( nullclines ) showNullclines()
			# plot out the trajectories
			# NEED TO DO BOTH FORWARD AND BACKWARD, maybe alpha different for backward, or darken a bit
			# here is the forward one
			for( k in 1:length(TS)) {
				if( !is.null(TS[[k]]$system)) {
					if( !is.null(TS[[k]]$forward) ){
						if(k==Ntraj){
							plotTraj( TS[[k]]$forward, col=Tcolors[k], lwd=2)
						}
						else{
							plotTraj( TS[[k]]$forward, col=Tcolors[k])
						}
					}  
					if( !is.null(TS[[k]]$back) ) {
						if(k==Ntraj){
							plotTraj( TS[[k]]$back, col=TcolorsBack[k], lwd=2)
						}
						else{
							plotTraj( TS[[k]]$back, col=TcolorsBack[k])
						}
					}
					goo <- TS[[k]]$init
					lpoints( goo[1], goo[2], col=Tcolors[k],pch=20)


				}
			}
		}
		PP<-xyplot(ylim~xlim, panel=myPanel, xlab=NULL, ylab=stateNames[2], main=list(paste(stateNames[1]), cex=.85), scales=list())
		print(PP, position=c(0.1,.48,.9,1), more=TRUE)
		suppressWarnings(print(port[[1]], position=c(0, .27, 1, .5), more=TRUE))
		suppressWarnings(print(port[[2]], position=c(0, 0, 1, .29), more=FALSE))
	}
	# =======
	manipulate( doPlot(xstart=xstart, ystart=ystart, 
					   Ntraj=Ntraj,tdur=tdur,tback=tback,
					   nullclines=nullclines,reviseWhat=reviseWhat,
					   flowWhat=flowWhat, doJacob=doJacob),
			   xstart = slider(xlim[1],xlim[2],step=diff(range(xlim))/200,init=mean(xlim),label=paste(stateNames[1], "Start")),
			   ystart = slider(ylim[1],ylim[2],step=diff(range(ylim))/200,init=mean(ylim),label=paste(stateNames[2], "Start")),
			   Ntraj = picker( One=1,Two=2,Three=3,Four=4,Five=5, initial="One",label="Current Trajectory"),
			   tdur = slider(0,100,init=10,label="Trajectory Duration"),
			   tback = slider(-100,0,init=0, label="Go back in time"),
			   nullclines = checkbox(initial=FALSE, label="Show Nullclines"),
			   reviseWhat = picker("None"=-1,"One"=1,"Two"=2, "Three"=3,
								   "Four"=4,"Five"=5,"All"=0, label="Revise DE for", initial = "None"),
			   flowWhat= picker("One"=1, "Two"=2, "Three" = 3, "Four"=4, "Five" = 5, 
								initial = "One", label="What flow to plot?"),
			   doJacob= picker("None"=0, "At Start"=1, "At Backward Limit"=2, "At Forward Limit"=3,
							   label="Jacobian", initial="None")
			   #              param1 = slider(.1,10,init=1,label="Parameter 1"),
			   #              param2 = slider(.1,10,init=1,label="Parameter 2")
			   )
}
