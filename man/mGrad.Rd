\name{mGrad}
\alias{mGrad}
\title{Interactive applet for exploring gradients}
\description{
Displays the level plot, contour plot, and gradient field of a given two-dimensional function. 
  }
\usage{
mGrad(expr, ..., xlim = c(0,10), ylim = c(0,10))
}
\arguments{
  \item{expr}{A mathematical formula of two variables of the form f(x,y)~x&y)}
  \item{\dots}{Extra arguments to be passed with \code{expr} as part of the mathematical function. Generally these are not evaluated.}
  \item{xlim}{The limits of the x axis.}
  \item{ylim}{The limits of the y axis.}
 
}
\details{
The gradient checkbox turns the gradient arrows on and off. Show partial w.r.t x and y checkboxes display the x and y components of the gradient vectors. Scale gradient units allows the arrows to be on a reasonable scale; at 1 they are their full width. Arrow spacing controls the number of gradient arrows on the screen, the smaller the spacing the more arrows there will be on the plot. Color palette merely allows the user to pick between 5 color schemes for the levelplot. Approximate number of contour lines is a slider that controls the number of contour lines through the \code{pretty} algorithm such that they are evenly spaced at nice intervals. Background smoothness chooses the width of the square grid upon which the calculations are performed. At large values the program runs slower, but has higher resolution. 
}
\value{
A function that implements and displays the level plot, contour plot, and gradient field of a given two-dimensional function.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if(require(manipulate)){
		mGrad(x*y~x&y)
		mGrad(sin(x*y)~x&y)
		mGrad(sin(cos(x)+y)~x&y)
		mGrad(cos(x)*sin(y)*y~x&y)
	}
}
\keyword{calculus}
\keyword{multivariate calculus}
