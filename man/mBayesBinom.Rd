\name{mBayesBinom}
\alias{mBayesBinom}
\title{Interactive applet to explore Bayesian estimates}
\description{
An applet which lets the user manipulate the prior distribution and sample size to explore its effect on the posterior distribution and Bayesian estimates.
}
\usage{
mBayesBinom(dat, ...)
}
\arguments{
\item{dat}{Binomial data consisting of zeros and ones to be used as the likelihood.}
\item{...}{Extra arguments will not be evaluated.}
}
\details{
The user specifies the prior distribution to use using the corresponding
picker. The beta distribution is the default as it is smooth and more
applicable to real life data, where our prior belief would drop exponentially
as distance increases from the mean, but it would not equal zero. When using
the beta distribution, the parameter 1 slider controls alpha, the parameter 2
slider controls beta. When using the uniform distribution as the prior,
parameter 1 controls the minimum and parameter 2 controls the maximum. Sample
size allows the user to sample any number of points from one point to twice the
size of the data set (the data set repeated).

Known bugs:
Logarithmic y-axis checkbox does not work, unfortunately. 
}

\value{
A function that allows the user to explore bayesian inference interactively.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if(require(manipulate)) {
		data(CPS)
		mBayesBinom(CPS$sex)
	}
}

\keyword{statistics}
