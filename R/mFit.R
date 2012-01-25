mFit = function(expr, data, instructor=rep(TRUE, 13), ...){
  if( !require(manipulate)) stop("Must use a manipulate-compatible version of R, e.g. RStudio")
  if (!require("mosaic")) stop("Must install mosaic package.")
  
  line.red = rgb(1,0,0,.6)
  
  xvar = as.character(expr[3])
  yvar = as.character(expr[2])
  xvals = data[[xvar]]
  yvals = data[[yvar]]
   
  f = list()
  f[[1]] = function(x,...) rep.int(1, length(x))
  f[[2]] = function(x,...) x
  f[[3]] = function(x,...) x^2
  f[[4]] = function(x,...) x^3
  f[[5]] = function(x,...) log(abs(x)+.000001)
  f[[6]] = function(x, k, ...) exp(k*x)
  # The sines and cosines MUST go at the end since they are duplicated with a slider
  f[[12]] = function(x, P, n, ...){
    res = matrix(0,nrow=length(x),ncol=n)
    for(j in 1:n) {res[,j] = sin(2*j*pi*x/P)}
    return(res)
  }
  f[[13]] = function(x, P, n, ...){
    res = matrix(0,nrow=length(x),ncol=n)
    for(j in 1:n) {res[,j] = cos(2*j*pi*x/P)}
    return(res)
  }
  
  mu1=0; mu2=0;mu3=0; mu4=0; mu5=0;
  a1=FALSE; a2=FALSE; a3=FALSE; a4 = FALSE; a5 = FALSE; a6 = FALSE; a7 = FALSE; a8 = FALSE; 
  a9 = FALSE; a10 = FALSE; a11 = FALSE; a12=FALSE; a13 = FALSE; a14 = FALSE;
 #####  
myPlot = function(k=k, n=n, P=P, mu1=mu1,mu2=mu2,mu3=mu3,mu4=mu4,mu5=mu5, 
                    sd=sd, a1=a1, a2=a2, a3=a3, a4=a4, a5=a5, a6=a6, a7=a7, a8=a8, 
                    a9=a9,a10=a10,a11=a11,a12=a12,a13=a13, ...){
  if(class(xvals)=="factor"){
    stop("Categorical explanatory variable in play! What do we do now? Treat it as numeric?")
    }
    
  f[[7]] = function(x, mu, sd, ...) pnorm(q = x, mean = mu1, sd = sd)
  f[[8]] = function(x, mu, sd, ...) pnorm(q = x, mean = mu2, sd = sd)
  f[[9]] = function(x, mu, sd, ...) pnorm(q = x, mean = mu3, sd = sd)
  f[[10]] = function(x, mu, sd, ...) pnorm(q = x, mean = mu4, sd = sd)
  f[[11]] = function(x, mu, sd, ...) pnorm(q = x, mean = mu5, sd = sd)
  
  .makeA = function(xx) {
     A = matrix(0,nrow=length(xx),ncol = sum(funchoice[1:11])) 
     # get rid of columns potentially for 7 and 8.  They are made with cbind()
     col.count = 1
     mu.count = 1
     for (fun.k in which(funchoice)) {
       if(fun.k %in% c(12,13)){
         newA = f[[fun.k]](xx,P=P,n=n)
         A = cbind(A,newA)
         col.count=col.count+ncol(newA)
       }
       else{
         A[,col.count] = f[[fun.k]](xx,k=k,P=P,mu=get(paste("mu",mu.count,sep="")),sd=sd,n=n)   
         col.count = col.count+1
         if(fun.k %in% 7:11) mu.count = mu.count+1   
       }
     } 
     return(A)
   }
   x = seq(min(xvals),max(xvals), length = 1000)
   
     funchoice = c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, 
                 a11, a12, a13)
  
   if( sum(funchoice)==0) {
     print("You must select at least one function to fit a curve!")
     bigy = 0*x
   }
   else{
     A = .makeA(xvals)
     bigA = .makeA(x)
     coefs = qr.solve(A, yvals)
     bigy = bigA %*% coefs
     predict.y = A %*% coefs
     RMS = abs(sqrt(mean((predict.y-yvals)^2))*diff(range(xvals)))
   }
  
  bigx=x #Avoid conflicting variable names
    mypanel = function(x, y){
      panel.xyplot(x, y, pch = 16)
      panel.xyplot(bigx, bigy, type = "l", col=line.red, lwd = 5)
#       grid.text(paste("RMS Error: ", signif(RMS, 3)), 
#                 x = unit(0, "npc")+unit(1, "mm"),
#                 y = unit(1, "npc")-unit(2, "mm"),
#                 just = "left",
#                 gp = gpar(col = "red", fontsize =10))
      }
  #PLOTTING F'REAL
      
     xyplot(yvals~xvals, data, xlab = xvar, ylab = yvar, panel = mypanel, 
            main = paste("RMS Error:", signif(RMS, 3)))

  }
 #####=============================
   
   labels=list("Constant", "x", "x^2", "x^3", "log(x)", "exp(kx)", 
               "pnorm1(mu1, sd)", "pnorm2(mu2, sd)",
               "pnorm3(mu3, sd)","pnorm4(mu4, sd)","pnorm5(mu5, sd)",
               "sin(2Pi*x/P)", "cos(2Pi*x/P)")
    controls = list(a1 = checkbox(TRUE, as.character(labels[1])))#const
     
    for (s in 2:length(instructor)){
            if( instructor[s] ) controls[[paste("a",s,sep="")]] = checkbox(FALSE, as.character(labels[s]))
            }
    if(instructor[6]) #exp
      controls$k=slider(-2,2, step = .05, initial=0.1)
    if(instructor[12]|instructor[13]){ #sin, cos
      controls$P=slider(.1,10, step = .01, initial = 5) 
      controls$n=slider(1,20, step = 1, initial = 1)} 
    for(b in 1:5){ #pnorms
        if(instructor[b+6])        
        controls[[paste("mu",b,sep="")]] = slider(min(xvals),max(xvals), step =.1, initial = min(xvals)+b*diff(range(xvals))/6)
    }
           if(any(instructor[7:11])) controls$sd = slider(0.1, diff(range(xvals))/2, step = .1, initial = diff(range(xvals))/4)
   
          
 
        manipulate(myPlot(k=k, n=n, P=P, mu1=mu1,mu2=mu2,mu3=mu3,mu4=mu4,mu5=mu5, 
                    sd=sd, a1=a1, a2=a2, a3=a3, a4=a4, a5=a5, a6=a6, a7=a7, a8=a8, 
                    a9=a9,a10=a10,a11=a11,a12=a12,a13=a13),
            controls
            )
}
