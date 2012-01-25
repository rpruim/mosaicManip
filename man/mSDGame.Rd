\name{mSDGame}
\alias{mSDGame}
\title{Interactive applet to explore the standard deviation}
\description{
A game where the user matches the standard deviation of the blue normal curve to the unknown standard deviation of the red curve to gain better intuition of what the standard deviation truly measures. 
}
\usage{
mSDGame()
}
\arguments{
}
\details{
The user attempts to match the standard deviation of the blue normal distribution to that of the red distribution using the Standard Deviation slider. When satisfied with their approximation, the user may check the show solution/next checkbox - this displays the normal distribution with the correct standard deviation in green. It displays the correct standard deviation as text, the user's approximation, and the percent off from the correct standard deviation. If the percent off is less than 20%, the user "wins", if more than 20%, the user "loses". 

The default red distribution is a random distribution. The possible red distributions include normal, uniform, exponential, t with 2 to 3 degrees of freedom, t with 3 degrees of freedom, F, and Chi-Squared. Using the distribution picker, the user may choose one distribution to use as the red unknown distribution. This can be used for more focused lessons on specific distributions, practice, or curiosity. 
}
\value{
A function that allows the user to explore standard deviations of different probability distributions.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
\dontrun{
mSDGame()
}
}
\keyword{statistics}
