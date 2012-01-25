mBias=function(expr, data){
  if(!require(manipulate)) 
	  stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  if (!require(lattice) | !require(grid)| !require(mosaic)) stop("Missing Packages.")
  
  A1=FALSE; A2=FALSE; A3=FALSE; A4=FALSE; A5=FALSE; A6=FALSE; A7=FALSE; 
           A8=FALSE; A9=FALSE; A10=FALSE; A11=FALSE; A12=FALSE; A13=FALSE; A14=FALSE;
           A15=FALSE; A16=FALSE; A17=FALSE; A18=FALSE; A19=FALSE; A20=FALSE
  checks=c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                 A15, A16, A17, A18, A19, A20)
  origMod=lm(expr, data)
  origCoefs=coef(origMod)
  yvar=as.character(expr[2])
  xvars.mod=attr(origMod$terms, "term.labels")   #xvars from original model
  
  xvars.data=names(data)
  xvars.data=xvars.data[which(xvars.data!=yvar)]  #all other variables not y in data
  #=============================myFun Starts!
  myFun=function(n, seed, signif, use.orig, checks){
    set.seed(seed)
    # fit the new model to the original data to set the scale for the axes
    # this makes it easier to see how the CI changes with seed and n and signif
    scaleData=data
    scaleData[[yvar]]=NULL
    scaleData = scaleData[which(checks)] # Just the selected variables
    scaleData[["newname"]] = data[[yvar]]
    scaleCIs = confint(lm( newname ~ ., data=scaleData), signif=0.95) # always 0.95 for reference
    
    if (use.orig) newData=data
    else newData=resample(data, n)
    foo = newData[[yvar]]
    newData[[yvar]]=NULL
    if (use.orig) newResids = resid(origMod)
    else newResids=resample(resid(origMod), n)
    # DTK gets rid of the prediction from the original model --- just the data tells
    # foo=predict(origMod, newdata=newData)+newResids
    newData=newData[which(checks)]
    newData[["newname"]]=foo
    newMod=lm(newname ~., data=newData)    #newMod from newData with only selected terms
    newCIs=confint(newMod,level=signif)
    limCIs=c(min(newCIs), max(newCIs))
    limCIs=c(min(newCIs)-.1*diff(range(limCIs)), max(newCIs)+.1*diff(range(limCIs)))
    levelNames=rownames(newCIs)
    coefs=c(rep(0, length(levelNames)))
    for(nm in names(origCoefs)){
      if(nm %in% levelNames){
        coefs[levelNames==nm]=origCoefs[names(origCoefs)==nm]
      }
    }
    
    plot( 1:2, type="n", ylim=.5+c(0,nrow(newCIs)),xlim=c(0,1),xaxt="n",yaxt="n",
         ylab="",xlab="",bty="n")
    
    
    for(b in 1:nrow(newCIs)){
      if( !is.na( coefs[b]) )
        draw.CI.axis( coefs[b], newCIs[b,1], newCIs[b,2], 1+nrow(newCIs)-b, levelNames[b],
                      low.for.scale=scaleCIs[b,1], high.for.scale=scaleCIs[b,2])
    }

  }
  #=========================myFun ends noooooo
  #=====================Begin draw.CI.axis function
    draw.CI.axis = function( true, low, high,k,name,low.for.scale=low,high.for.scale=high){
      left = min(true,low.for.scale) - 0.66*abs(high.for.scale-low.for.scale)
      right = max(true,high.for.scale) + 0.66*abs(high.for.scale-low.for.scale)
      ticks = signif(pretty( c(left,right), n=5),5)
      axis( side=1,pos=k,at=seq(0,1,length=length(ticks)),
          labels=paste(ticks))
      # convert the units
      axis.left = min(ticks)
      axis.right = max(ticks)
      do.convert = function(val,left,right){ (val - left)/abs(right-left)}
      true = do.convert(true, axis.left, axis.right)
      low = do.convert(low, axis.left, axis.right)
      high = do.convert(high, axis.left, axis.right)
      lines(c(true,true),k+c(-.2,.2),lty=3,col="black")
      points(mean(c(low,high)),k, pch=20,col="red",cex=2)
      text( c(low,high),c(k,k),c("(",")"),col="red")
      lines( c(low,high),c(k,k),lwd=3,col=rgb(1,0,0,.5))
      text( 0.05, k, name, pos=3, xpd=NA) # xpd about clipping.
    }
    #=====================End draw.CI.axis function        
  first.val = ncol(data)*3+20 # heuristic to get enough degrees of freedom
  step.size = round( max(1,nrow(data)/200)) # heuristic to get a nice number of steps
  first.val = first.val - ((first.val + ncol(data)) %% step.size)
  controls=list(n=slider(first.val, 5*nrow(data), step=step.size, initial=nrow(data), label="n points to sample"))
  for(a in 1:length(xvars.data)){
    controls[[paste("A", a, sep="")]]=checkbox(FALSE, label=xvars.data[a])
  }
  controls$seed = slider(100,500,step=1, initial=sample(100:500,1), label="Random seed")
  controls$signif = slider(.5,.99,step=.01, initial=0.95, label="Significance level")
  controls$use.orig = checkbox(initial=FALSE,label="Use Original Data")
  
  manipulate(myFun(n=n, seed=seed, signif=signif, use.orig=use.orig, checks=c(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14,
                 A15, A16, A17, A18, A19, A20)),
             controls)
}
