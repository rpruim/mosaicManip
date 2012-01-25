mEulerIntegrate <- function() {
	if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
	tryCatch( .mEulerIntegrate.core(), 
			 error=function(e){ 
				 stop(paste(e,"Need newer version of RStudio", collapse="\n"))
			 }
	)
}

.mEulerIntegrate.core = function(){
  if( !require(manipulate) ) 
	  stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  # Built in dynamical functions
  logistic = function(x,t=0){.2*x*(1-x)}
  exponential = function(x,t=0){ -.8*x }
  gompertz =function(x,t=0){exp(-t/3)*.2*x}
  cooling=function(x,t=0){-.1*(x-.7)}
  cows=function(v,t=0){H=10; r=1/3; K=25; beta=0.1; V0=3; r*v*(1-v/K) - H*beta*v^2/(V0^2+v^2)}
  pills=function(x,t=0){dose=1; dur=0.5; interval=5; -0.2*x + dose*(((t+1)%%interval)<dur)} 
  trajs = list(NULL, NULL, NULL)
  tcur = 0
  xcur = 0
  colorlist = c("red","blue","black")
  # ============draw the state
  draw.state = function(dynfunname="logistic",xval0=.5,
    dt=1, ntraj=1,restart=FALSE,nextstep=FALSE,
    go,tcenter=5,twidth=5,xwidth=1.5,xcenter=.5,nsteps=1,
                        showequilibria=FALSE,editfun) {
    # Set the function
    # edit the function
    if (editfun ){
      if( dynfunname=="linear" ) exponential <<- edit(exponential)
      if( dynfunname=="logistic") logistic <<- edit(logistic)
      if( dynfunname=="Gompertz") gompertz <<- edit(gompertz)
      if( dynfunname=="Newton Cooling") cooling <<- edit(cooling)
      if( dynfunname=="cows") cows <<- edit(cows)
      if( dynfunname=="pills") cows <<- edit(pills)
    }
    if( dynfunname=="linear" ) dynfun = exponential
    if( dynfunname=="logistic") dynfun = logistic
    if( dynfunname=="gompertz") dynfun = gompertz
    if( dynfunname=="Newton Cooling") dynfun = cooling
    if( dynfunname=="cows") dynfun = cows
    if( dynfunname=="pills") dynfun = pills
    
    # Update the current traj
    foo = trajs[[ntraj]]
    if( is.null(foo) || restart || xval0 != foo$x[1]) {
      foo = list(x=xval0,t=0)
    }
    else if(go) {
      for (k in 1:nsteps) {
        npts = length(foo$x)
        foo$t[npts+1] = foo$t[npts]+dt
        foo$x[npts+1] = foo$x[npts] + dt*dynfun(foo$x[npts],foo$t[npts])
      }
    }
    trajs[[ntraj]] <<- foo # put it back into the general storage
    #Figure out the time and x-scale
    tmin = 0
    tmax = 10
    xmax = 1.2
    xmin = -.2
    for( k in 1:length(trajs)) {
      if( !is.null(trajs[[k]])) {
        tmax = max( tmax, max(1.2*trajs[[k]]$t + dt*nsteps))
        xmax = max( xmax, max(1.2*trajs[[k]]$x))
        xmin = min( xmin, min(trajs[[k]]$x))
        }
    }
    plot( 0:1, type='n',
          ylim=c(xmin,xmax)+0.05*c(-1,1)*abs(xmax-xmin),
          xlim=c(0,tmax),
          ylab="State x",xlab="Time t")
   
    # draw the integration line
    npts = length( foo$x )
    if( npts > 1 ) { # There is a trajectory
      slope = dynfun( foo$x[npts-1],foo$t[npts-1])
      lines( foo$t[npts-1] + c(-3,3), 
             foo$x[npts-1]+ slope*c(-3,3),
             col="black")
    }
  
    #draw the trajectories
    for (k in 1:length(trajs)){
      foo = trajs[[k]]
      if (!is.null(foo)){ #draw it!
        points( foo$t, foo$x, col=colorlist[k], pch=20,cex=.5)  
        lines( foo$t, foo$x, col=colorlist[k])
      } 
    }
    # Show the equilibria
    if( showequilibria){
     ..thisfun <<- function(x){dynfun(x,0)}
     eqs = findZeros( ..thisfun(x)~x, xlim=c(xmin,xmax))
     for (k in 1:length(eqs)) {
       lines( c(tmin,tmax),c(1,1)*eqs[k], col=rgb(0,0,0,.4), lwd=5)
     }
    }
    
}
  manipulate(draw.state(dynfunname=dynfun,xval0=xval0,dt=dt,ntraj=ntraj,go=go,restart=restart,nsteps=nsteps,showequilibria=showeq,editfun=editfun),
             dynfun=picker("linear","logistic","Newton Cooling","cows","pills","Gompertz",label="Dynamics",initial="logistic"), 
             go = button("GO!"),
             dt = slider(0.05,10.0,step=0.05,init=0.5,label="Euler Stepsize: dt"),
             nsteps=slider(1,20,init=1,label="Number of Euler steps to take"),
             xval0 = slider(-.5,3.5,step=0.01,init=.5,label="Initial value: x0"),
             showeq = checkbox("Show Equilibria",initial=FALSE),
             ntraj = picker("red"=1,"blue"=2,"black"=3, label="Active Trajectory"),
             restart=button(label="Start Over"),
             editfun = button( label="Edit the Dynamical Function")
             )
}
