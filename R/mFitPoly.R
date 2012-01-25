mFitPoly=function(expr, data, ...){
  instruct=c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,FALSE,FALSE,FALSE,
             FALSE,FALSE,FALSE,FALSE)
  mFit(expr, data, instructor=instruct, ...)
}