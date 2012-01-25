mPhaseLine <- function(dyn=function(x){x*(1-x)}, xlim=c(-.2,1.2)){
	if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
	xpts <- seq(xlim[1],xlim[2],length=500) 
	ypts <- dyn(xpts)
	inds <- ceiling(rank(-ypts))
	ninds <- max(inds)
	#arrange the colors so that zero always has a fixed color
	nLessThanZero <- max( inds[ypts<0])
	colors <- c(rainbow(start=0,end=.5,n=ninds-nLessThanZero,alpha=.75),
			   rainbow(start=.5,end=1,n=nLessThanZero,alpha=.75))
	#colors <- topo.colors(ninds+round(length(xpts)/5),alpha=.75)


	drawGraph <- function(dt,x,field=FALSE,npts=6,showDyn=FALSE){
		type <- ifelse(showDyn,"p","n") # plotting type
		ylabel <- ifelse(showDyn,"d State / dt","")
		plot( xpts, ypts, col=colors[inds],pch=20,
			 xaxt="n",yaxt="n",bty="n",
			 xlim=xlim,
			 ylim=c(min(0,min(ypts)),max(0,max(ypts))),
			 main="Dynamics",cex=.75,xlab="State",ylab=ylabel,type=type)


		pointAtX <- function(x,dt=0.1){
			xind <- max(1,sum( x>=xpts )) #Where does it fall in the x-sequence
			cinds <- inds[xind]
			lines( c(x,x+dt*dyn(x)), c(0,0),col=colors[cinds],lwd=10,xpd=NA)
			text(x+dt*dyn(x), 0, ifelse(dyn(x)>0,">","<"),col=colors[cinds],cex=2)
			if( showDyn) {
				lines( c(x,x,min(xlim)),c(0,dyn(x),dyn(x)),col=colors[cinds],lty=3,lwd=2)
				points(min(xlim),dyn(x),cex=2,col=colors[cinds],pch=20)
			}
			points(x,0,pch=20,col="black")
			points(x,0,pch=20,cex=2,col=colors[cinds])
			#arrows( min(xlim),0, min(xlim),dyn(x),code=2,col=colors[cinds], lwd=5,length=.1)
		}
		if( field ) {
			edge <- abs(diff(xlim))/10
			states <- seq(xlim[1]+edge,xlim[2]-edge,length=npts)
			for(k in states) pointAtX(k, dt)
		}

		pointAtX(x,dt)

		axis(1,pos=0,col="red")
		if( showDyn ) axis(2, pos=min(xlim),col="black")

	}
	manipulate( drawGraph(dt,x,field=field,npts=npts,showDyn=showDyn),
			   x = slider( xlim[1],xlim[2],step=abs(diff(xlim))/100,init=mean(xlim),label="State Value"),
			   field = checkbox(init=TRUE,label="Draw Flow Field"),
			   npts = slider(1,20,init=6,label="Number of Field Points"),
			   dt = slider(0,2,step=0.01, init=.5,label="dt"),
			   showDyn = checkbox(label="Show dx/dt Function")
			   )
}
