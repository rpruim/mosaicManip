
# manipulate 2nd order poly of two vars

mpoly2 <- function(){
	if( !require(manipulate) ) 
		stop("Must use a manipulate-compatible version of R, e.g. RStudio")
	showFun <- function(a1,a2,a3,a4,a5,a6,xlim,surface=FALSE){
		.polyfun <<- function(x,y){
			a1 + a2*x + a3*y + a4*x*y + a5*x^2 + a6*y^2
		}
		plotFun(.polyfun(x,y)~x&y,
				x=c(-2^xlim,2^xlim), 
				y=c(-2^xlim,2^xlim),
				surface=surface)
	}
	manipulate( showFun(a1,a2,a3,a4,a5,a6,xlim,surface),
			   a1=slider(-1.0,1.0,step=.01,initial=0,label="const"),
			   a2=slider(-1.0,1.0,step=.01,initial=0,label="x"),
			   a3=slider(-1.0,1.0,step=.01,initial=0,label="y"),
			   a4=slider(-1.0,1.0,step=.01,initial=0,label="xy"),
			   a5=slider(-1.0,1.0,step=.01,initial=0,label="x^2"),
			   a6=slider(-1.0,1.0,step=.01,initial=0,label="y^2"),
			   xlim=slider(-4,20,step=1,initial=2,label="Scale range (log2)"),
			   surface=checkbox(initial=FALSE,label="Surface")
			   )
}
