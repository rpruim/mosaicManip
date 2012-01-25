#' Interactive applet to explore integrals as applied to economics
#' 
#' An applet applying integrals to the economic concepts of supply and demand.
#' 
#' Two splines are created from the sliders. The first points of the spline are
#' anchored at x=0, y=0 for the supply spline, and x=0, y=50 for the demand
#' spline. The sliders allow the user to manipulate the y points which
#' determine the splines at x=10 and x=20. The area between the equilibrium
#' point and the splines is shaded. In perfect competition, the area above the
#' supply spline and below the equilibrium point is the producer surplus, and
#' the area below the demand spline and above the equilibrium point is the
#' consumer surplus.
#' 
#' Known bugs: If the curves do not intersect as may happen when demand pt. 2
#' is high and supply pt. 2 is low, there will be an error: Error in
#' uniroot(func, interval = c(0, 20)) : f() values at end points not of
#' opposite sign However it's better for the exercise to be more flexible than
#' less.
#' 
#' @param xlim The range of points to plot on the Quantity axis. By default, it
#' is set to be c(0,20).
#' @return A function that allows the user to explore integrals as applied to
#' economics.
#' @author Andrew Rich (\email{andrew.joseph.rich@@gmail.com}) and Daniel
#' Kaplan (\email{kaplan@@macalester.edu})
#' @keywords calculus economics
#' @examples
#' 
#' 	if(require(manipulate)){
#' 		mEcon()
#' 	}
#' 
mEcon <- function(xlim=c(0,20)){
  if( !require(manipulate) ) 
	  stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  if ( !require(lattice) | !require(grid) ) 
	  stop("Missing packages.")

  x <- seq(min(xlim), max(xlim), length=1000)
  myFun <- function(sup1, sup2, dem1, dem2){
    supFun <- splinefun(c(0,mean(x),max(x)), y=c(0, sup1, sup2)) 
    demFun <- splinefun(c(0,mean(x),max(x)), y=c(50, dem1, dem2))
    func <- function(x){supFun(x)-demFun(x)}
    equil <- uniroot(func, interval=xlim) #Solve for equil qty.
    xEquil <- equil$root
    yEquil <- supFun(xEquil)
    Supply <- supFun(x)          #making my life easy with auto.key by naming vars Supply and Demand
    Demand <- demFun(x)
    xx <- x     #To avoid duplication in the polygons in the panel function
    panel.Econ <- function(x,y,...){
      panel.xyplot(x,y,...)
      panel.lines(x=c(xEquil, -999999), y=c(yEquil, yEquil), lty=2, col="black")
      panel.lines(x=c(xEquil, xEquil), y=c(yEquil, -999999), lty=2, col="black")
	  # controlPoints <- data.frame(x=c(mean(x), max(x), mean(x), max(x)), y=c(sup1, sup2, dem1, dem2), spline=rep('supply','demand', each=2))
      panel.points(x=c(0, mean(x), max(x)), y=c(0, sup1, sup2), col="blue", cex=1.5)
      panel.points(x=c(0, mean(x), max(x)), y=c(50 , dem1, dem2), col="red", cex=1.5)
      leftx <- xx[xx<=xEquil]
      ySup <- c(supFun(leftx), rep(yEquil, length(leftx)))
      xpts <- c(min(leftx),leftx,max(leftx))
      panel.polygon(x=c(leftx, rev(leftx), min(leftx)), y=c(rep(yEquil, length(leftx)), supFun(rev(leftx)), yEquil ), 
               col=rgb(0,0,.8,.2), border=FALSE)
      yDem <- c(demFun(leftx), rep(yEquil, length(leftx)))
      panel.polygon(x=c(min(leftx), leftx, max(leftx)), y=c(yEquil, demFun(leftx), yEquil), 
               col=rgb(.8,0,0,.2), border=FALSE)

      
    }
    xyplot(Supply+Demand~x, auto.key=list(lines=TRUE,points=FALSE, columns=2), xlab="Quantity", ylab="Price",
           type="l", lwd=3, panel=panel.Econ)
  }
  manipulate(myFun(sup1=sup1, sup2=sup2, dem1=dem1, dem2=dem2),
             sup1=slider(1,40, step=.01, initial=25, label="Supply when quantity = 10"),
             sup2=slider(1,60, step=.01, initial=50, label="Supply when quantity = 20"),
             dem1=slider(20, 60, step=.01, initial=25, label="Demand when quantity = 20"),
             dem2=slider(0, 40, step=.01, initial=5, label="Demand when quantitye = 20")
             ) 
}
