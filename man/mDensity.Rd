\name{mDensity}
\alias{mDensity}
\title{Interactive applet to explore density plots}
\description{
An applet which lets the user manipulate the bandwidth of a density plot and apply different transformations to examine how density plots reflect smoothed data. 
}
\usage{
mDensity(data=NULL)
}
\arguments{
\item{data}{Numerical data to be used in the density plot. Only one variable of numerical data is allowed. If null, mDensity creates random data from the specified distribution.}
}
\details{
The user may specify the bandwidth (amount of smoothing) on density plot using the bandwidth slider. Transformations may be performed on the data using the transformation picker. These transformations include log, square root, rank, and inverse cosine. If data is null, three more options are available from the manipulate box. The distribution picker allows the user to choose the type of distribution for random data to be created from. The random seed slider picks the random seed. Number of points chooses the number of points to be generated as data. 

Known bugs:
Some transformations return NaNs and display an error in the console. The density plot does not plot those which are NaNs. 
}
\value{
A function that allows the user to explore density plots and transformations on data interactively.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if(require(manipulate)){
		data(trees)
		mDensity(trees$Girth)
		mDensity()
	}
}
\keyword{statistics}
