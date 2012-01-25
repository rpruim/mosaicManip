\name{mEcon}
\alias{mEcon}
\title{Interactive applet to explore integrals as applied to economics}
\description{
An applet applying integrals to the economic concepts of supply and demand. 
}
\usage{
mEcon(xlim=c(0,20))
}
\arguments{
  \item{xlim}{The range of points to plot on the Quantity axis. By default, it       is set to be c(0,20).}
}
\details{
Two splines are created from the sliders. The first points of the spline are anchored at x=0, y=0 for the supply spline, and x=0, y=50 for the demand spline. The sliders allow the user to manipulate the y points which determine the splines at x=10 and x=20. The area between the equilibrium point and the splines is shaded. In perfect competition, the area above the supply spline and below the equilibrium point is the producer surplus, and the area below the demand spline and above the equilibrium point is the consumer surplus.

Known bugs:
If the curves do not intersect as may happen when demand pt. 2 is high and supply pt. 2 is low, there will be an error:
Error in uniroot(func, interval = c(0, 20)) : 
  f() values at end points not of opposite sign
However it's better for the exercise to be more flexible than less. 
}
\value{
A function that allows the user to explore integrals as applied to economics.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if(require(manipulate)){
		mEcon()
	}
}
\keyword{calculus}
\keyword{economics}
