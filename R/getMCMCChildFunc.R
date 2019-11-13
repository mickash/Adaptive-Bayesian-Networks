#' @keywords internal
getMCMCChildFunc<-function(node,parent,sample) {
  return (UseMethod('getMCMCChildFunc',node))    
}
#' @keywords internal
getMCMCChildFunc.dirichlet <- function (node,parent,sample) {
  p<-which(node$parents==parent)
  val=sample[node$index]
  pvals=sample[node$parents]
  probs<-rep(1,pvals[p])
  for (i in 1:pvals[p]) {
    pvals[p]=i
    probs[i]=prob(node,pvals,val,"nat")
  }
  f<-function(input) {
    probs
    return(input*probs)
  }
  return (f)
}
#' @keywords internal
getMCMCChildFunc.flag <- function (node,parent,sample) {
  stop("Logical nodes should not be calling MCMC Child function generation functions.")
}
#' @keywords internal
getMCMCChildFunc.logand <- function (node,parent,sample) {
  stop("Logical nodes should not be calling MCMC Child function generation functions.")
}
#' @keywords internal
getMCMCChildFunc.noisyor <- function (node,parent,sample) {
  p<-which(node$parents==parent)
  val=sample[node$index]
  pvals=sample[node$parents]
  # We are performing natural sampling - so no auxiliary parents are involved
  probs<-rep(1,pvals[p])
  for (i in 1:pvals[p]) {
    pvals[p]=i
    probs[i]=prob(node,pvals,val,aux=NULL,met="nat")
  }
  f<-function(input) {
    probs
    return(input*probs)
  }
  return (f)
}
#' @keywords internal
getMCMCChildFunc.logprod <- function (node,parent,sample) {
  stop("Logical nodes should not be calling MCMC Child function generation functions.")
}
#' @keywords internal
getMCMCChildFunc.flagprod <- function (node,parent,sample) {
  stop("Logical nodes should not be calling MCMC Child function generation functions.")
}
#' @keywords internal
getMCMCChildFunc.logor <- function (node,parent,sample) {
  stop("Logical nodes should not be calling MCMC Child function generation functions.")
}
