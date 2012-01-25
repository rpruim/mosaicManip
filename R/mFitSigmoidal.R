mFitSigmoidal=function(expr, data, ...){
  instruct=c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,TRUE,TRUE,TRUE,
             TRUE,TRUE,FALSE,FALSE)
  mFit(expr, data, instructor=instruct, ...)
}