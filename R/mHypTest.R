mHypTest <-
function(useF=FALSE){
  if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  maxeffect=10
  resid = 10 # just to avoid a missing value.  It's reset by the controller.
  controls = list()
  if( useF) controls[["effect"]]=slider(0,.99,step=.01,init=.2,label="Effect Size (R^2)")
  else controls[["effect"]] = slider( -maxeffect, maxeffect, step=.1,init=-1, label="Effect size (coef.)")
  controls[["n"]] =  slider(1,100, step=1,init=50,label="n-m (resid d.f.)")
  if( useF ) controls[["m"]] = slider(1,20,step=1, init=3,label="m (# of model vectors)")
  if( !useF) controls[["resid"]] = slider(1,100,init=10,label="Residual Size")
  controls[["signif"]] = slider(0.01,0.2,step=.01,init=0.05,label="Significance")
  if (!useF) controls[["ang"]] = slider(-90,90,init=90,label="Covariate angle")
  controls[["show.alt"]] = checkbox(FALSE,label="Show Alternative Hyp.")
  
  
    
  drawIt <- function(effect=5,n=10,ang=90,resid=10,m=1,signif=.05,
   col.null=rgb(0,0,1,.5), col.alt=rgb(1,0,0,.5),show.alt=TRUE, one.sided=FALSE){
   if( useF ){ # convert R^2 to F
     effect = (n-m)/(m)*(effect/(1-effect))
   }
   if( useF ){ # use the F distribution and do things one-sided
   dnorm = function(vals, mean=1, sd=1) {
     df(vals-mean, m, n)
    }
    pnorm = function(vals, mean=1, sd=1) {
     pf(vals-mean, m, n)
    }
    qnorm = function(vals, mean=1, sd=1) {
     abs(effect) + qf(vals, m, n-1)
    }
   }
   else { # use the t distribution
    dnorm = function(vals, mean=0, sd=1) {
     dt((vals-mean)/sd, n)
    }
    pnorm = function(vals, mean=0, sd=1) {
     pt((vals-mean)/sd, n)
    }
    qnorm = function(vals, mean=0, sd=1) {
     mean + sd*qt(vals, n-1)
    }
   }
  
  SE = min(1e6,abs(1/sin(ang*pi/180)))*resid/sqrt(n)
  if (useF) {
    topval = max(df(seq(.05,1,length=10),m,n))*1.1
    xrange = c(-0.5,ifelse(show.alt,effect,0) + qf(.99, m, n) )
  }
  else { # t distribution
   topval=dnorm(0,sd=SE)*1.1 #fix this to be the highest value in the window
   xrange = max( c(maxeffect+3*c(1,-1)*SE, 3*c(1,-1)*SE,2*maxeffect ),1.5*qnorm(1-signif/2,sd=SE) )*c(-1,1)
  }
  xpts = seq(min(xrange)-diff(xrange),max(xrange)+diff(xrange),length=1000)
  left.threshold = c(min(xrange)-diff(xrange), qnorm(signif/2, mean=0, sd=SE) )
  if (useF) {
    right.threshold = c( qf(1-signif, m,n), max(xrange) + diff(xrange) )
  }
  else{
   right.threshold = c(qnorm(1-signif/2, mean=0, sd=SE), max(xrange)+diff(xrange) ) 
  }
  #plot out the null hypothesis
  null.pts = dnorm( xpts, mean=0, sd=SE )
  plot( xpts, null.pts, type="l", col="blue",
      xlab="Test Statistic", ylab="Prob. Density",
      xlim=xrange, ylim=c(0,topval + exp(-topval/.1)))
      text( 0, .2*dnorm( 0, sd=SE), "Null", col="blue", pos=3)
  # and the alternative hypothesis
  if( show.alt ) {
   alt.pts = dnorm(xpts, mean=effect, sd=SE)
   lines(xpts, alt.pts, col="red")
   text( effect, .1*dnorm( 0, sd=SE), "Altern.", col="red", pos=3)
  }
  # Draw the significance
  draw.significance = function(threshold, lim,center=0,col=col.null) {
    xpts = seq(threshold, lim, length=1000)
    ypts = dnorm(xpts, mean=center,sd=SE)
    goo = par("usr")
    polygon(c(threshold,xpts),c(0,ypts),col=col, border=NA )
    polygon(c(threshold,threshold,lim,lim,threshold), c(0,10*topval,10*topval,0,0), col=rgb(0,0,0,.1))
    if (lim > threshold ) text( threshold,mean(goo[c(3,4,4,4,4)]),"Rej. thresh.",srt=-90,pos=4) 
    else text( threshold, mean(goo[c(3,4,4,4,4)]),"Rej. thresh.", srt=90, pos=2)
    return( diff( pnorm( range(c(threshold,10000*lim)), mean=center, sd=SE) ) )
}

  sig=  draw.significance( right.threshold[1],right.threshold[2] )
  if( !useF ) sig = sig + draw.significance( left.threshold[2], left.threshold[1] ) 
  # draw the power
  if( show.alt ) {
   power = draw.significance( right.threshold[1],right.threshold[2],center=effect,col=col.alt )
   if( !useF ) power = power + draw.significance( left.threshold[2], left.threshold[1], center=effect, col=col.alt )
   goo = par('usr')
   text(ifelse(useF,1,0),goo[4], 
    paste("Power=", signif(power,3),sep=""),
     pos=1)
  }
  }
  ang=90 #just in case there's no "ang" control
  manipulate(drawIt(effect=effect,n=n,ang=ang,resid=resid,signif=signif,m=m,show.alt=show.alt), 
   controls)
  
}

