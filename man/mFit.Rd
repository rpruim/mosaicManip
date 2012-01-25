\name{mFit}
\alias{mFit}
\title{Interactive applet for term selection and nonlinear parameters for fitting.}
\description{
Displays data and a menu of modeling functions.  
Finds a linear combination of the selected modeling functions, with the user setting nonlinear parameters manually.
}
\usage{
mFit(expr, data, instructor=rep(TRUE, 13), ...)
}
\arguments{
  \item{expr}{A formula object giving the dependent and independent variables to use.}
  \item{data}{A data frame containing the values for the variables identified in \code{expr}.}
  \item{instructor}{A logical vector consisting of \code{TRUE} and \code{FALSE} that tells the program which checkboxes to display. The vector is length 13. The order of terms to which \code{instructor} corresponds is as follows:
  1. constant
  2. x
  3. $x^2$
  4. $x^3$
  5. log(x)
  6. exp(k*x)
  7. pnorm1
  8. pnorm2
  9. pnorm3
  10. pnorm4
  11. pnorm5
  12. sin(x)
  13. cos(x)
  }
  \item{...}{Extra arguments to be passed to the functions described in \code{instructor}. Mostly these will not be evaluated.}
}
\details{
This function attempts to fit a linear combination of the selected terms to the data plotted. Terms are selected by clicking the corresponding checkbox. There are sliders only for nonlinear parameters.  Linear coefficients are found automatically.  Only a function of one variable is supported at present. By checking each checkbox, the term is added to the fitted function, and the linear fit of the selected terms is updated. The instructor argument allows an instructor to hide certain terms such that a lesson may be geared towards specific subjects without distraction. \code{mFitSines}, \code{mFitSigmoidal}, and \code{mFitPoly} are premade wrappers that select only the constant, x, and sines and cosines terms, the pnorm terms, and the polynomial terms, respectively. 
}
\value{
A function that implements the current state of the parameters and terms selected.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if(require(manipulate)){
		data(KidsFeet)
		mFit(width ~ length, data=KidsFeet)
	}
}
\keyword{calculus}
