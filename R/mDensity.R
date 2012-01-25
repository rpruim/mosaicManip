mDensity=function(data=NULL){
  if(!require(manipulate)) 
	  stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  if (!require(lattice)) 
	  stop("Missing packages.")

  #=================
  myFun=function(bandwidth, dist, trans, npts, ...){
    if(is.null(data)){
      if(dist==1)
        x=rnorm(npts, mean=0, sd=5)
      if(dist==2)
        x=runif(npts, min=-10, max=10)
      if(dist==3)
        x=rexp(npts, rate=1)
     }
    else
       x=data
    trans.x=trans(x)
    densityplot(trans.x, xlab= "Values", bw=bandwidth,
                scales = list(x=list(cex=1.5)))
  }
  #===================
  controls=list(bandwidth=slider(.01, 5, initial=1, step=.01, label="Bandwidth"),
                trans=picker("None"=c, "Log"=log, "Sqrt"=sqrt, "Rank"=rank, "arcCos"=acos, label="Transformation")
             )     
    if(is.null(data)){
      controls[["dist"]]=picker("Normal"=1, "Uniform"=2, "Exponential"=3, label="Distribution")
      controls[["seed"]]=slider(1, 100, step=1, label="Random seed", initial=1)
      controls[["npts"]]=slider(10,1000, step=1, initial=100, label="Number of points")
    }
  #=====================
  manipulate(myFun(bandwidth=bandwidth, dist=dist, trans=trans, seed=seed, npts=npts),
             controls)
}
