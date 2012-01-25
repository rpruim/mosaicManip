#' Interactive applet for exploring the SIR epidemic disease model
#' 
#' Displays a phase portrait of the number of susceptible, infected, and
#' recovered populations against \code{t}
#' 
#' Using the sliders, a user may control the probability of infection, the
#' number of contacts per day, duration of disease decay, the number of the
#' initial population that is infected, and finally the number of births per
#' day. With these parameters, the model can be substantially altered. For
#' example, if the probability of infection is high and contact is high, the
#' disease sweeps through the population quickly. If probability of infection
#' and contact are low, the disease may not infect every member of the
#' population. If there are many babies added per day and a long disease decay,
#' sometimes a recurring epidemic occurs. The model starts with a population of
#' 1000 initially and runs for 100 days.
#' 
#' @param lwd width of lines used in plots
#' @param type type of plot to display.  See \code{\link{xyplot}} for possible
#' values of this argument.
#' @param ... additional arguments passed to \code{\link{xyplot}}.
#' @return A function that implements and displays the current state of each
#' group in the SIR model at many points in time.
#' @author Andrew Rich (\email{andrew.joseph.rich@@gmail.com}) and Daniel
#' Kaplan (\email{kaplan@@macalester.edu})
#' @keywords differential equations epidemiology SIR
#' @examples
#' 
#' 	if(require(manipulate)){
#' 		mSIR()
#' 	}
#' 
mSIR <- function(lwd=3, type='l', ...){
  if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  sir <- function(p=.1, ncontact=10, N=1000, births=0, gamma=.2, Iinit=1, ndays=100, dt=.1 ){
  npoints = ceiling( ndays/dt )
  S <- rep(0, npoints)
  I <- rep(0, npoints)
  R <- rep(0, npoints)
  t <- rep(0, npoints)
  S[1] <- N - Iinit;
  I[1] <- Iinit;

  for (k in 2:npoints) {
    conversions <- p*(ncontact/N)*I[k-1]*S[k-1]
    dS <- -conversions + births
    dI <- conversions - gamma*I[k-1]
    dR <- gamma*I[k-1]
    S[k] <- S[k-1] + dt*dS
    I[k] <- I[k-1] + dt*dI
    R[k] <- R[k-1] + dt*dR
    t[k] <- t[k-1] + dt
  }

  return( data.frame(S=S, I=I, R=R, t=t))
}
  myFun <- function(probInfect, nContact, duration, initialI, births){
    dat <- sir(p=probInfect, ncontact=nContact, gamma=duration, Iinit=initialI, births=births)
    lattice::xyplot(S+I+R~t, data=dat, auto.key=list(points=FALSE, lines=TRUE, columns=3), lwd=lwd, type=type,
		   ...)
  }
  if (require(manipulate)) {
  manipulate(myFun(probInfect=probInfect, nContact=nContact, duration=duration, initialI=initialI,
                   births=births),
             probInfect = slider(0, 1, step=.001, label="Probability of Infection", initial=.1),
             nContact = slider(0, 100, step=1, label="Contacts per day", initial=10),
             duration = slider(.01, 1, step=.001, label="Duration of disease decay", initial=.2),
             initialI = slider(1, 50, step=1, label="Initial Infected", initial=1),
             births = slider(0, 20, step=1, label="Births/day", initial=0)
             )
  }
}
