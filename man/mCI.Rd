\name{mCI}
\alias{mCIprop}
\alias{mCIt}
\title{Interactive applets to explore confidence interval coverage}
\description{
These applets allow the user explore the coverage of confidence 
intervals computed from many random samples.
}

\usage{
mCIprop(...)
mCIt(...)
}

\arguments{
	\item{...}{additional arguments}
}

\details{

Data for \code{mCIprop} is created using \code{rbinom}. The user may choose to
calculate confidence intervals using a Wald interval or an Agresti interval by
using the type of CI picker. There are sliders in the manipulate box to
interact with sample size, confidence level, true mean, number of trials, and
random seed. Number of trials increases the number of lines plotted. The total
number of confidence intervals not containing the true mean, or "failed CIs",
is printed above the plot. 

Known bugs: Color scheme may be redone, orange/chocolate1 may not be the best
color for failed CIs.  Wald CIs "error" when true mean is near the edge of the
line and sample size is low, however this is more a product of the Wald CI
calculation than the program.
}
\value{
A function that allows the user to explore confidence interval coverage interactively.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if (require(manipulate)) {
		mCIprop()
		mCIt()
	}
}

\keyword{statistics}
