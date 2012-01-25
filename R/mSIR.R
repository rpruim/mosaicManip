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
