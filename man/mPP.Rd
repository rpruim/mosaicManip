\name{mPP}
\alias{mPP}
\title{Interactive applet for differential equation modeling.}
\description{
Displays a phase plane of two variables and their phase portraits against \code{t}
  }
\usage{
mPP(DE=predator.prey, xlim=c(-10,2000),ylim=c(-10,2000))
}
\arguments{
  \item{DE}{A function object containing the differential equation to be solved.}
  \item{xlim}{The boundaries of the x axis in the phase plane.}
  \item{ylim}{The boundaries of the y axis in the phase plane.}
}
\details{
Sliders control the initial conditions of both variables. The trajectory picker allows the user to edit a new trajectory, saving the previous one to compare against. The trajectory duration and go back in time sliders control the duration of the time the system is allowed to run for, forwards and backwards in time. Show Nullclines checkbox displays the nullclines as transparent polygons on the phase plane. Revise DE allows the user to edit the differential equation for one trajectory at a time. If the DEs are different, the plot flow picker allows the user to display the arrow field of the desired trajectory. The Jacobian picker prints the Jacobian matrix and its eigenvalues at the initial point, at the end of the duration, or at the beginning of the duration. The parameter sliders are currently nonfunctional.
}
\value{
A function that implements and displays the current state of the variables in the DE at many points in time.
}
\author{
Andrew Rich (\email{andrew.joseph.rich@gmail.com}) and Daniel Kaplan (\email{kaplan@macalester.edu}) 
}

\examples{
	if(require(manipulate)){
		mPP(DE=predator.prey)
	}
}
\keyword{differential equations}
