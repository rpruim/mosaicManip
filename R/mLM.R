mLM <- function(formula, data=NULL, ...) {
	if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
	mod <- lm( formula, data=data, ...)
	response.name <- deparse(mod$terms[[2L]])
	term.labels <- attr(mod$term, "term.labels")

	col.to.term <- (mod$assign)
	if( attr(mod$term, "intercept") == 1 ){
		term.labels <- c("Intercept", term.labels)
		col.to.term <- col.to.term + 1
	}
	keep.vector <- (1:length(term.labels))
	mat <- model.matrix(mod)
	response <- fitted(mod) + resid(mod)
	if (length(term.labels) > 20)
		stop("Too many terms.  Ask DTK to fix this.")

	controls <- list(xname=picker(as.list(names(mod$model)[-1]),label="X-axis"))
	for (k in 1:length(term.labels) ) {
		controls[[ paste("a",k,sep="") ]] <- checkbox( TRUE, label=term.labels[k] )
	}

	yscale <- range(response)
	yscale <- yscale -diff(yscale)*(c(-1,1)*.05)
	#
	# graphics function
	ffun <- function(xname,vec) {
		n <- length(term.labels)
		keep.columns <- col.to.term %in% keep.vector[ vec[1:n] ]
		if ( sum(keep.columns)==0  ) {
			yscale <- range(c(response,0)) #include 0 in the scale
			newvals <- 0*response
		} 
		else {
			newmat <- mat[,keep.columns]
			if (!is.matrix(newmat) ) newmat <- cbind(newmat) 
			# make sure it's a matrix.
			#eliminate redundant columns
			qq <- qr(newmat)
			keep <- 1e-6 < abs(diag(qr.R(qq)))
			newmat <- qr.Q(qq)%*%(qr.R(qq)[,keep])
			newvals <- newmat %*% qr.solve( newmat, response, tol=1e-15 )
		}
		xvar <- mod$model[[xname]]
		if( is.factor( xvar ) ){
			xxvar <- jitter(as.numeric(xvar) )
			# DO THE PLOT labeling the x-axis with the levels of the factor
			plot( xxvar, newvals,pch=20,
				 ylab=response.name,xlab=xname, col="black",xaxt="n",ylim=yscale)
			axis(1, at= 1:length(levels(xvar)), labels=levels(xvar) )
		}
		else {
			xxvar <- xvar # just for symmetry with the is.factor case above.
			plot( xxvar, newvals,pch=20,
				 ylab=response.name,xlab=xname, col="black",ylim=yscale)
		}
		points(xxvar, response, pch=20, col=rgb(0,0,1,.3) )
		foo <- par("usr")
		goo <- par("pin")
		text(foo[1],foo[4]+.05*(foo[4]-foo[3]),
			 paste("R^2=",signif(var(newvals)/var(response),3)),pos=4,xpd=NA)
	}
	a1=TRUE;a2=TRUE;a3=TRUE;a4=TRUE;a5=TRUE;a6=TRUE;a7=TRUE;a8=TRUE;a9=TRUE;a10=TRUE
	a11=TRUE;a12=TRUE;a13=TRUE;a14=TRUE;a15=TRUE;a16=TRUE;a17=TRUE;a18=TRUE;a19=TRUE;a20=TRUE
	manipulate(ffun(xname,c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)), controls)

}
