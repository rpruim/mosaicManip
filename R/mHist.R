mHist=function(data=NULL){
  if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  newCol=rgb(0,.2,.8,.6)
  oldCol=rgb(.9,.9,0,.3)
  .iter=0
  if(.iter==0)
    oldBreaks<<-NULL
  
  #===============
  myFun=function(nBands, cBreak, seed){
  if(is.null(data))  set.seed(seed)
    .iter<<-.iter+1
  if(is.null(data)){
    x=rnorm(500, mean=0, sd=5)
    }
  else
    x=data
    
    rng=diff(range(x))
    myby=rng/(nBands)
    c.rng = diff(c(min(x),cBreak))
#     minratio= c.rng/rng
#     maxratio= 1-minratio      #I believe these lines are not necessary. 
    myBreaks = seq(cBreak, (max(x)+rng/5), by=myby)
    myBreaks = c(seq(cBreak, (min(x)-rng/5), by=-myby), myBreaks)
    
    myPan=function(x,...){
       panel.histogram(x, breaks=myBreaks, col=newCol)
        if(!is.null(oldBreaks)){
         panel.histogram(x, col=oldCol, breaks=oldBreaks, border=FALSE)
        }
      }
    
    newHist=histogram(x, panel=myPan, type = "density",
                      scales = list(x=list(cex=1.7)))
    print(newHist)
    oldBreaks<<-myBreaks   #Keep the old breaks for next time
  }
       controls = list(nBands=slider(4, 50, step=1, initial=10, label="Number of histogram bins"),
             cBreak=slider(mean(x)-3,mean(x)+3, step=.01, initial=signif(mean(x), 3), label="Center Break")
             )
      if(is.null(data))   #seed slider only shows up when data is null
        controls$seed=slider(1,100,step=1, initial=1, label="Random seed")

 #========================
  manipulate(myFun(nBands=nBands, cBreak=cBreak, seed=seed),
             controls
             )
  
  
}
#Alpha slider for the overlay
#if data not null, kill seed
#pretty up the slider for center break
