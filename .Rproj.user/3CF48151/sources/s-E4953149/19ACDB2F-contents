#' @keywords internal
gen_MB <- function (object,children,net,sample,aux,met="aux") {
  return (UseMethod('gen_MB', object))    
}
#' @keywords internal
gen_MB.dirichlet <- function (object,children,net,sample,aux,met) {
  pvals<-sample[object$parents]
  row <- calculate.row(pvals,object$pvals)
  probs<-object$params[row,]
  if (!is.null(children[[object$name]])) {
    for (i in children[[object$name]]) {
      probs=adjustParentProbability(net$nodes[[i]],object$index,sample,probs)    
    }    
  }
  genFromRow(probs)  
}
#' @keywords internal
gen_MB.flag <- function (object,children,net,sample,aux,met) {
  stop("Logical nodes should not be calling MB generation functions.")
}
#' @keywords internal
gen_MB.logand <- function (object,children,net,sample,aux,met) {
  stop("Logical nodes should not be calling MB generation functions.")
}
#' @keywords internal
gen_MB.noisyor <- function (object,children,net,sample,aux,met) {
  if (identical(met,"nat")) {
    return (gen_MB_raw.noisyor(object,children,net,sample))
  } else {
    return (gen_aux.noisyor(object,aux))    
  }
}
#' @keywords internal
gen_MB_raw.noisyor <- function (object,children,net,sample) {
  pvals<-sample[object$parents]
  if (length(object$values)!=2) {
    stop("Raw value generation not permitted for multivariate noisy or nodes.")    
  }
  fail=object$params[1,2]/sum(object$params[1,2:3])
  if (length(object$parents)>0) {
    for (i in 1:length(object$parents)) {
      if (pvals[i]>1){
        row=object$pindices[i]+pvals[i]-2
        fail=fail*object$params[row,2]/sum(object$params[row,2:3])
      }
    }          
  }
  probs=c(fail,1-fail)
  if (!is.null(children[[object$name]])) {
    for (i in children[[object$name]]) {
      probs=adjustParentProbability(net$nodes[[i]],object$index,sample,probs)    
    }    
  }
  genFromRow(probs)  
}
#' @keywords internal
gen_MB_aux.noisyor <- function (object,children,net,sample,aux) {
  stop("Auxiliary based noisy ors are logical nodes and so should not be calling MB generation functions.")
}
#' @keywords internal
gen_MB.logprod <- function (object,children,net,sample,aux,met) {
  stop("Logical nodes should not be calling MB generation functions.")
}
#' @keywords internal
gen_MB.flagprod <- function (object,children,net,sample,aux,met) {
  stop("Logical nodes should not be calling MB generation functions.")
}
#' @keywords internal
gen_MB.logor <- function (object,children,net,sample,aux,met) {
  stop("Logical nodes should not be calling MB generation functions.")
}



adjustParentProbability<-function(node,parent,sample,probs) {
  p<-which(node$parents==parent)
  val=sample[node$index]
  pvals=sample[node$parents]
  probs2<-rep(1,node$pvals[p])
  for (i in 1:node$pvals[p]) {
    pvals[p]=i
    probs2[i]=prob(node,pvals,val,NULL,"nat")
  }
  probs*probs2
}
