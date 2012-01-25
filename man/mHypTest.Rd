\name{mHypTest}
\alias{mHypTest}
\title{
Interactive display for hypothesis testing concepts.
}
\description{
Displays a sampling distribution and calculates power.
}
\usage{
mHypTest(useF=FALSE)
}
\arguments{
  \item{useF}{Show the F distribution.  Default: a t-distribution for the sample statistic.}
}
\details{
Allows the user to set various parameters that govern the sampling distribution, and the 
significance, then calculates the power.  Displays all this graphically.
}
\value{
Just the graphics as a side effect.
}
\author{
Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if(require(manipulate)){
		mHypTest()
		mHypTest(useF=TRUE)
	}
}
\keyword{statistics}
