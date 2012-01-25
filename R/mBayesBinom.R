mBayesBinom=function(dat, ...){  
  if(!require(manipulate)) 
	  stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  if (!require("mosaic")) stop("Must install mosaic package.")
  
  nthetapts=1000  #Resolution in theta for thetapts

  #Log Likelihood function:
  LL = function(theta,ns,Ntrials){
    ns*log(theta)+(Ntrials-ns)*log(1-theta)
  }
  posterior.poly = rgb(1,.5,0,.2)
  posterior.text = rgb(1,.5,0,1)
  
  
  myFun = function(pick.prior=pick.prior, min=min, max=max, samp.size=samp.size, log.yaxis=log.yaxis){
    theta.pts = seq(0, 1, length = nthetapts)
    uniform =function(theta){
      if(min>max) {cat("Don't cross the streams!")}
      dunif(theta, min = min, max=max)}
    beta = function(theta){
      min = min*10; max=max*10;
      dbeta(theta, min, max)
    }
    prior=list(uniform, beta)
    prior.pts = prior[[pick.prior]](theta.pts)

	baseCat <- sort(unique(dat))[1]
    #Replicate data so you can sample more than exists! 
    dat = rep(dat, times = 2)
    nsuccesses=sum( dat[1:samp.size] == baseCat ) 
    LL.pts = LL(theta.pts,nsuccesses,samp.size)
    likeli.pts = exp(LL.pts)
    posterior.pts = prior.pts*likeli.pts
    posterior.pts = posterior.pts/mean(posterior.pts)
    mypanel = function(x,y){
      lpolygon(c(0,x,1),y=c(0,y,0), col=rgb(0,0,1,.1), border = FALSE)
      lpolygon(c(0,x,1), c(0,posterior.pts,0), col = posterior.poly, border=FALSE)  
      scaled.likeli = likeli.pts/max(likeli.pts)*max(prior.pts)
      llines(x, scaled.likeli, col = "red", lwd = 2)
      #Likelihood text
      x.like.max = x[which(scaled.likeli==max(scaled.likeli))]
      grid.text(x= unit(x.like.max,"native"), y = unit(max(scaled.likeli)*0.7,"native"), label = "Likelihood", rot = 270, gp = gpar(col = "red"))
      ltext(x=x.like.max, y = max(scaled.likeli)*1.2, 
            label= paste("theta =", signif(x.like.max, 3),
                         "\nProb =", signif( max(likeli.pts), 3)),
            col = "red")
      #Prior Text:
      succex = which(prior.pts != 0)
      succex = x[succex]
      if(pick.prior==1){
        x.prior = succex[10]
        ltext(x= x.prior, y = .9*prior[[pick.prior]](x.prior), col = "blue",
            label="Prior", pos = 4)
       }
      if(pick.prior==2){
        x.prior=x[which(prior.pts==max(prior.pts))]
        ltext(x=x.prior, y = .9*prior[[pick.prior]](x.prior), col = "blue",
            label="Prior")
      }
      #Posterior Text:
      x.post.max = x[which(posterior.pts ==max(posterior.pts))]
      y.post.max = posterior.pts[which(posterior.pts ==max(posterior.pts))]
      ltext(x=x.post.max, y = 1*y.post.max, col = posterior.text, label = paste("Posterior:\n",
                                                                       expression(theta),"=",signif(x.post.max,3),
                                                                       "\nProb =", signif(y.post.max,3)))
      
    }
    
    xyplot(prior.pts~theta.pts, panel = mypanel, ylim = c(0,1.1*max(posterior.pts)), 
           xlab = expression(theta),ylab = "Probability Density",
           scales = list(x = list(log = FALSE), log=log.yaxis))
    #The scales argument should accept a list, and x and y can be separate lists.
    #On the internet I see many instances of y=list(log=TRUE) being the correct syntax. Not sure what's up.
  }
  
  manipulate(myFun(pick.prior=pick.prior, min=min, max=max, samp.size=samp.size, log.yaxis=log.yaxis),
             pick.prior= picker("Uniform" = 1, "Beta"=2, label = "Prior Distribution", initial = "Beta"),
             min = slider(0.1, 1, initial = .3, label = "Parameter 1"), #don't cross the streams!
             max = slider(0.1, 1, initial = .8, label = "Parameter 2"),
             samp.size = slider(1, 2*length(dat), label = "Sample Size", initial=.4*length(dat)),
             log.yaxis = checkbox(FALSE, label = "Logarithmic y-axis")
             )
}
