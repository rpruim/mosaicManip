#' Interactive applet to explore density plots
#' 
#' An applet which lets the user manipulate the bandwidth of a density plot and
#' apply different transformations to examine how density plots reflect
#' smoothed data.
#' 
#' The user may specify the bandwidth (amount of smoothing) on density plot
#' using the bandwidth slider. Transformations may be performed on the data
#' using the transformation picker. These transformations include log, square
#' root, rank, and inverse cosine. If data is null, three more options are
#' available from the manipulate box. The distribution picker allows the user
#' to choose the type of distribution for random data to be created from. The
#' random seed slider picks the random seed. Number of points chooses the
#' number of points to be generated as data.
#' 
#' Known bugs: Some transformations return NaNs and display an error in the
#' console. The density plot does not plot those which are NaNs.
#' 
#' @param data Numerical data to be used in the density plot. Only one variable
#' of numerical data is allowed. If null, mDensity creates random data from the
#' specified distribution.
#' @return A function that allows the user to explore density plots and
#' transformations on data interactively.
#' @author Andrew Rich (\email{andrew.joseph.rich@@gmail.com}) and Daniel
#' Kaplan (\email{kaplan@@macalester.edu})
#' @keywords statistics
#' @examples
#' 
#' 	if(require(manipulate)){
#' 		data(trees)
#' 		mDensity(trees$Girth)
#' 		mDensity()
#' 	}
#' 
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
