#' Interactive applet to explore power of a hypothesis test
#' 
#' A group activity meant to demonstrate the concept of power of a hypothesis
#' test. When played as a class, students make a sound when they reject the
#' null hypothesis, so that gradually the whole class makes the designated
#' noise as power becomes large.
#' 
#' The histogram represents the random sample data based on n points centered
#' around mu with standard deviation of sigma. The true mean is 5. As mu
#' increases, power increases. As n increases, power increases. As sigma
#' increases, power decreases. The class should use this applet together and
#' move the sliders to designated points together. If a student is able to
#' reject the null hypothesis, they make a sound such as "ribbit". As power
#' increases due to changes to n, mu, and sigma, ribbits increase.
#' 
#' @return A function that allows the user to explore power.
#' @author Andrew Rich (\email{andrew.joseph.rich@@gmail.com}), Daniel Kaplan
#' (\email{kaplan@@macalester.edu}), and Homer White
#' (\email{Homer_White@@georgetowncollege.edu})
#' @keywords statistics
#' @examples
#' 
#' 	if(require(manipulate)){
#' 		mPower()
#' 	}
#' 
mPower <- function()  {
  if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  data <- NULL
  #=================
  myFun <- function(mu=mu, sigma=sigma, n=n)
  { #set.seed(seed)
    samp <- rnorm(n,mean=mu,sd=sigma)
    mu0 <-5
    x.bar <- mean(samp)
    margin <- qt(0.975,df=n-1)*sd(samp)/sqrt(n)
    low <- x.bar-margin
    high <- x.bar+margin
    t.stat <- (x.bar-mu0)*sqrt(n)/sd(samp)
    p.value <- round(2*(1-pt(abs(t.stat),df=n-1)),3)
    histogram(samp,ylab="Density", type="density",
        xlim=c(0,10),xaxt="n",xlab="",
        main="Power of a Hypothesis (t) Test",
        sub=paste("P-value = ",p.value),
        col=rgb(0,.5,.9,.7),
        panel=function(x,...){
          panel.histogram(x,...)
          #panel.densityplot(x, col="orange")
          llines(x=c(low,high), y=c(0,0), col="red", lwd=10)
          lpoints(x=x.bar, y=0, col="green", cex=1.3, pch=19)
          grid.text(label=expression(mu), x=unit(x.bar,"native"), y=unit(.1, "npc"))
          lpoints(x=mu0, y=0, col="black", cex=1.3, pch=19)
          grid.text(label=expression(mu[0]==5), x=unit(mu0,"native"), y=unit(.03, "npc"))
        })
    }
#=====================
  controls=list( mu=slider(5,7,step=0.10,initial=6, label="Mu"),
                 sigma=slider(0,4,step=0.1,initial=2, label="Sigma"),
                 n=slider(5,305,step=5,initial=50, label="N sample points")
                  )
  #controls$seed=slider(1,100, step=1, initial=1)
                 
  manipulate(myFun(mu=mu, sigma=sigma, n=n),
             controls)
}
