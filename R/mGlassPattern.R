mGlassPattern = function(npts=3000,seed=NULL){
  if( !require(manipulate) ) 
	  stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  if(!is.null(seed)) set.seed(seed)
    xpts = runif(npts,min=-1.4,max=1.4)
    ypts = runif(npts,min=-1.4,max=1.4)
    sizes = exp(-runif(npts,min=-.2,max=1.4))
  
    transform = function(xpts,ypts,mag,dmag,rot,xtrans,ytrans,dt){
      cs = cos( rot*pi/180); sn = sin(rot*pi/180)
      nx = (mag+dmag)*xpts
      ny = (mag-dmag)*ypts
      nx = (1-dt)*xpts + dt*(cs*nx + sn*ny + xtrans)
      ny = (1-dt)*ypts + dt*(-sn*nx + cs*ny + ytrans)
      return( list(x=nx,y=ny))
      
    }
    plotPts = function(mag=1,dmag=0,rot=0,xtrans=0,ytrans=0,dt=1) {
      p = list( x=c(1,0),y=c(0,1))
      p = transform(p$x,p$y,mag=mag,dmag=dmag,rot=rot,xtrans=xtrans,ytrans=ytrans,dt=dt)
      mat = cbind( p$x, p$y)
      evals = signif(eigen(mat)$values,4)
      plot( xpts, ypts, cex=sizes,
            xlim=c(-1,1),ylim=c(-1,1),
            main=paste("Eigenvalues:",evals[1],"and",evals[2]),
            xlab="",ylab="",xaxt="n",yaxt="n",pch=20,col=rgb(0,0,0,.5))
      p = list(x=xpts,y=ypts)
      for( k in 1:5 ) {
        p = transform(p$x,p$y,mag=mag,dmag=dmag,rot=rot,xtrans=xtrans,ytrans=ytrans,dt=dt)
        points( p$x, p$y, cex=sizes-.5+k/10,pch=20,col=rgb(0,0,0,.5))
      }
      p=list(x=c(-.5,0,.5,-.5,0,.5,-.5,0,.5)*1.5,
             y=c(-.5,-.5,-.5,0,0,0,.5,.5,.5)*1.5)
      for (k in 1:10 ) {
        p = transform(p$x,p$y,mag=mag,dmag=dmag,rot=rot,xtrans=xtrans,ytrans=ytrans,dt=dt)
        points( p$x, p$y, pch=20,col=rgb(1,0,0,k/10),cex=1.05^k)
      } 
    }
  
    manipulate( plotPts( mag=mag, dmag=dmag,rot=rot,xtrans=xtrans,ytrans=ytrans,dt=dt),
                mag=slider(.8,1.2,step=0.01,init=1,label="Common Magnification"),
                dmag=slider(-.2,.2,step=0.01,init=0,label="Delta Magnification"),
                rot=slider(-10,10,step=0.25,init=0,label="Rotation (degrees)"),
                xtrans=slider(-.1,.1,step=0.001,init=0,label="Horizontal Translation"),
                ytrans=slider(-.1,.1,step=0.001,init=0,label="Vertical Translation"),
                dt = slider(0,1,step=.01,init=.5,label="dt")
                )
  
}
