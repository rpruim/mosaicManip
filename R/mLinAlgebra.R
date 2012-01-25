mLinAlgebra <- function(...,A=NULL,b=NULL, target=b) {
	if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
	# set up the plotting coordinates to give a square, so right angles look right.
	inches <- par("pin")
	par(pin=c(1,1)*min(inches))
	# Set up the vectors in the right format.
	if(is.null(target) )
		stop("Must spectify argument target=")
	target <- cbind(target)
	if( is.null(A) )  
		vecs <- list(...)
	else {
		for (k in 1:ncol(A))
			vecs[[k]] <- c(A[,k])
	}

	two <- nrow(target)==2
	three <- nrow(target)==3

	if( !(two | three) ) stop("All vectors must be in either two or three dimensions.")

	if( two ) { # two dimensional vectors
		for (k in 1:length(vecs)) {
			if( length(vecs[[k]]) !=2 ) stop("All vectors must have the same length.")
			vecs[[k]] <- c(vecs[[k]],0)
		}
		target <- c(target,0)
	}
	if(three) {
		target <- c(target)
		for (k in 1:length(vecs)){
			if( length(vecs[[k]]) != 3) stop("All vectors must have the same length.")
		}
	}
	vector.choices <- c("None",paste("Vector",1:length(vecs)))

	# Preliminary definitions

	# ====================
	find.scale <- function(){
		biggest <- sqrt(sum(target^2))
		for (k in 1:length(vecs)) {
			sz <- sqrt(sum(vecs[[k]]^2) )
			if( sz > biggest) biggest <- sz
		}
		return(biggest)
	}
	long <- find.scale()
	# ====================
	trans3d <- function(vec,pmat,...) {
		# my version without perspective
		tmat <- vec%*%pmat[1:3,1:2];
		return( list(x=tmat[,1], y=tmat[,2]) )
	}
	Lseg <- function(v1,v2,pmat,...){
		trans3d(matrix(c(v1,v2), nrow=2,byrow=TRUE),pmat,...)
	}

	# ===============
	rotmatrix <- function(az, el) {
		cbind( c(cos(az), sin(az), 0, 0 ), 
			  c(-sin(el)*sin(az), sin(el)*cos(az), cos(el), 0),
			  c(cos(el)*sin(az), -cos(el)*cos(az), sin(el), 0),
			  c(0,0,0,1));
	}

	# ====================
	showvecs <- function(zoom=1,scales=c(1,1,1),
						az=30,el=-15,lookdown="None",
						hint=FALSE) {
		if( lookdown != "None") {
			vec.number <- which( lookdown == vector.choices) - 1
			v <- vecs[[vec.number]]
			az <- 360-atan2(v[1],v[2])*180/pi;
			el <- -atan2(v[3],sqrt(v[1]^2+v[2]^2))*180/pi;
		}

		rotmat <- rotmatrix( pi*az/180, pi*el/180)
		colors <- rainbow(max(5,length(vecs)+1))
		targetcolor <- "blue"
		plot(1:2,xlim=long*c(-1,1)/zoom, ylim=long*c(-1,1)/zoom, type="n", xaxt="n",yaxt="n",
			 xlab="",ylab="")

		# draw the axes
		big <- zoom*long
		lines(Lseg( c(-big,0,0),c(big,0,0), rotmat ),lty=2)
		lines(Lseg( c(0,-big,0),c(0,big,0), rotmat ),lty=2)
		lines(Lseg( c(0,0,-big),c(0,0,big), rotmat ),lty=2)
		# plot the target
		points(trans3d( target, rotmat), col=colors[1],pch=20,cex=2)
		# plot the subspaces
		offset <- c(0,0,0)
		big <- 1000*big
		for (k in 1:length(vecs) ) {
			lines( Lseg( offset-big*vecs[[k]], offset+big*vecs[[k]],rotmat), lwd=1, col=colors[k+1])
			lines( Lseg( offset, offset + scales[k]*vecs[[k]], rotmat), lwd=5, col=colors[k+1])
			#lines( Lseg( offset, offset + vecs[[k]], rotmat), lwd=10, col=colors[k+1])
			offset = offset + scales[k]*vecs[[k]]
		}
		offset <- c(0,0,0)
		for ( k in 1:length(vecs) ) {
			points( trans3d( offset+vecs[[k]], rotmat), pch=20,cex=2, col=colors[k+1])
			text( trans3d( offset+.5*vecs[[k]], rotmat), paste(k))
			offset = offset + scales[k]*vecs[[k]]
		}
		# keep the offset value
		points( trans3d( offset, rotmat), col=colors[1], pch=10, cex=2)
		foo <- par("usr")
		goo <- par("pin")
		text(foo[1],foo[3]+.05*(foo[4]-foo[3]),
			 paste("Sum Sq. Resids=",signif(sum((target-offset)^2),3)),pos=4)
		# show the hint
		if (hint) {
			soln <- find.initial.scales(soln=TRUE)
			# Give a hint of the worst one
			hintnum <- which.max( abs(soln - scales[1:length(vecs)] ) )
			text(foo[1],foo[4]-.05*(foo[4]-foo[3]),
				 paste("Try vector", hintnum, "=",signif(soln[hintnum],3)), pos=4)

		}
	}

	# Figure out an appropriate scale for the vectors.
	find.initial.scales <- function(soln=FALSE){
		A <- cbind()
		for (k in 1:length(vecs) ) A <- cbind(A, vecs[[k]])
		ss <- qr.solve(A, target)
		if( soln ) {
			return( ss )
		}
		x <- pmax(1.5,abs(ss))
		sz <- ceiling(2^(ceiling( log2(x)+.75 )))
		return(sz)
	}


	# Create the manipulator controls
	controls <- list()
	sz <- find.initial.scales()
	for( k in 1:(length(vecs)) ) {
		controls[[paste("s",k,sep="")]] = 
		slider( -sz[1],sz[1],initial=1,step=0.05,label=paste("Vector",k," multiplier"))
	}
	controls[["hint"]] <- checkbox(FALSE,label="Hint")
	if(three) controls[["lookdown"]] = picker(as.list(vector.choices),initial="None",label="Look down a vector")
	controls[["zoom"]] <- slider(.1, 4, initial=1, step=.1,label="Zoom in or out")
	if(three) controls[["az"]] <- slider(0,90,initial=45,label="Viewpoint Azimuth")
	if(three) controls[["el"]] <- slider(-90,90,initial=-15, label="Viewpoint Elevation")   


	s1=1;s2=1;s3=1;s4=1;s5=1;s6=1;s7=1;s8=1;s9=1;s10=1;
	if(three){
		manipulate( showvecs(scales=c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10), 
							 zoom=zoom,az=az,el=el,lookdown=lookdown,hint=hint), controls)
	}
	if(two) {
		manipulate( showvecs(scales=c(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10), 
							 zoom=zoom,az=0,el=90,lookdown="None",hint=hint), controls)
	}
}
