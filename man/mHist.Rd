\name{mHist}
\alias{mHist}
\title{Interactive applet to explore histograms}
\description{
An applet which lets the user manipulate the number of bins and the center break of a histogram corresponding to data given or a random normal distribution.
}
\usage{
mHist(data=NULL)
}
\arguments{
\item{data}{Numerical data to be plotted as a histogram. Only one variable of numerical data is allowed. If null, mHist creates random data from the normal distribution.}
}
\details{
The user may change the number of bins using the Number of Histogram bins slider, and they may change the center point around which the bins are centered with the center break slider. If data is null, the user may change the random seed with the seed slider. Otherwise the seed slider is not displayed. 

Known bugs:
Due to the way breaks in the histogram are calculated, for very small numbers of bins (around less than 4) an error appears on the plot, as the breaks don't cover all the data. This may also occur for random seeds occasionally. 
}
\value{
A function that allows the user to explore histograms and their comparative advantages and disadvantages for displaying scalar data.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if(require(manipulate)){
		data(trees)
		mHist(trees$Girth)
		mHist()
	}
}
\keyword{statistics}
