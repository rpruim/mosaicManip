\name{mFitExp}
\alias{mFitExp}
\title{Interactive applet for term selection and setting nonlinear parameters to fit data with exponential  and logarithmic functions.}
\description{
Displays data and a menu of modeling functions. 
Finds a linear combination of the selected modeling functions, with the user setting nonlinear parameters manually. A wrapper for \code{mFit}, see mFit for more details.
}
\usage{
mFitExp(expr, data, ...)
}
\arguments{
  \item{expr}{A formula object giving the dependent and independent variables to use.}
  \item{data}{A data frame containing the values for the variables identified in \code{expr}.}
  \item{...}{Additional arguments to be passed to \code{mFit}
  }
}
\details{
A premade wrapper for \code{mFit} such that only the constant, x, exponential, and log terms are displayed. See \code{mFit} for more details. 
}
\value{
A function that implements the current state of the parameters and terms selected.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if(require(manipulate)){
		hawaii=fetchData("hawaii.csv")
		mFitExp(Girth~Volume, data=trees)
	}
}
\keyword{calculus}
