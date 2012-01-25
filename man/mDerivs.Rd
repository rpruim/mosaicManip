\name{mDerivs}
\alias{mDerivs}
\title{Manipulate Applet for Displaying Functions}
\description{
This applet displays a plot of a function, its derivative, and its antiderviative. 
}
\usage{
mDerivs(expr, xlim=c(0,10), ...) 
}
\arguments{
  \item{expr}{a mathematical expression. See mosaic \code{D}.
}
  \item{xlim}{ The range of the dependent variable.
  }
  \item{\dots}{ additional arguments to \code{expr}
  }
}
\details{
Displays a function defined by \code{expr}, along with the derivative and integral of that function.  Allows the user to see how those three functions are related by slope and by area, visually. The user may manipulate the position of x (\code{xpos}) and the point at which the integral is calculated from (\code{from}). The user may also turn off the display of the derivative and integral plots with the corresponding checkboxes, if desired.
}
\value{
A function.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu}) and 
Andrew Rich (\email{andrew.joseph.rich@gmail.com})
}

\examples{
\dontrun{
mDerivs((cos(x))~x, xlim = range(0, 10))
mDerivs(sin(exp(x))~x, xlim = range(-2, 3))
}
}
\keyword{ calculus }
