\name{mApproxPoly2}
\alias{mApproxPoly2}
\title{Interactive applet for exploring fitting multivariate models}
\description{ Displays a shaded level and contour plot. A circle in which z
	values are fitted based on the best linear combination of terms selected is
	overplotted. Root mean squared error of the fit is displayed as the title.
}

\usage{
mApproxPoly2(expr, ..., xlim = c(0,1), ylim = c(0,1))
}
\arguments{
  \item{expr}{A mathematical formula of two variables of the form f(x,y)~x&y)}
  \item{\dots}{Extra arguments to be passed with \code{expr} as part of the mathematical function. Generally these are not evaluated.}
  \item{xlim}{The limits of the x axis.}
  \item{ylim}{The limits of the y axis.}
}

\details{
The x and y sliders change where in the plane the circle is centered, and the
radius slider adjusts how large the circle of fitted values is. Each checkbox
adds the indicated term to the fitted function. The circle transparency slider
controls the transparency of the circle, where 1 is opaque and 0 is invisible -
it is an alpha transparency. Number of pixels controls how many points are in
the square grid upon which the function is calculated. \code{mApproxPoly2} may
run slower at higher pixel counts. Approximate number of contour lines is a
slider that controls the number of contour lines through the \code{pretty}
algorithm such that they are evenly spaced at nice intervals.
}
\value{
A function that implements and displays the colored level plot contour plot of a given two-dimensional function, and plots a best fit function with selected variables in a transparent circle over the plot.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan
(\email{kaplan@macalester.edu}) 
}

\examples{
if(require(manipulate)) {
mApproxPoly2(sin(x*y)~x&y)
mApproxPoly2(sin(x*y)+cos(x)~x&y, xlim=c(0,5), ylim=c(0,5))
}
}
\keyword{calculus}
\keyword{multivariate calculus}
