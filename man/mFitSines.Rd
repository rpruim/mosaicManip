\name{mFitSines}
\alias{mFitSines}
\title{Interactive applet for term selection and setting nonlinear parameters to fit data with sines and cosines.}
\description{
Displays data and a menu of modeling functions.  
Finds a linear combination of the selected modeling functions, with the user setting nonlinear parameters manually. A wrapper for \code{mFit}, see \code{mFit} for more details.
}
\usage{
mFitSines(expr, data, ...)
}
\arguments{
  \item{expr}{A formula object giving the dependent and independent variables to use.}
  \item{data}{A data frame containing the values for the variables identified in \code{expr}.}
  \item{...}{Additional arguments to be passed to \code{mFit}
  }
}
\details{
A premade wrapper for \code{mFit} such that only the constant, x, and the sine and cosine terms are displayed. See \code{mFit} for more details. 
}
\value{
A function that implements the current state of the parameters and terms selected.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if(require(manipulate)){
		fetchData("hawaii.csv")-> hawaii
		mFitSigmoidal(water~time, data=hawaii)
		hawaii=fetchData("hawaii.csv")
		mFitSines(water~time, data=hawaii)
	}
}


\keyword{calculus}
