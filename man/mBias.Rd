\name{mBias}
\alias{mBias}
\title{Interactive applet to explore integrals as applied to economics}
\description{
An applet investigating model bias
}
\usage{
mBias(expr, data)
}
\arguments{
\item{expr}{A model formula that is taken as the original model to be evaluated.}
\item{data}{A dataset containing the terms in \code{expr}.}
}
\details{
The original model is created from the original data set and the model given by \code{expr}. The response variable is taken to be the term on the left side of your original formula. All other terms are considered explanatory, and they are displayed as checkboxes in the manipulate box. By checking these, each term is added to the new model. The old model coefficient is plotted as a dotted line on each number line, and the new confidence interval and coefficient is plotted in red. The user may vary the confidence level and may vary the number of data points resampled to change the placement and size of the confidence intervals. There is also a original data checkbox which disables the n points to sample slider, as original data is used. When original data is checked as well as only the terms in the original model, the confidence intervals should align with the original predictions. There is a random seed slider to set the random seed. By default its initial value is random.

This applet allows users to see that a linear model will be drastically different with different terms in it. A confidence interval is only applicable to its own model or hypothesis test, it is not universally valid. 
}
\value{
A function that allows the user to explore bias in linear models with respect to confidence intervals and coefficients.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
if(require(manipulate)) {
data(CPS)
mBias(wage~sex+educ+exper, data=CPS)
}
}
\keyword{statistics}
