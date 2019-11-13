#' Generate random value
#' 
#' S3 generic function for generating a random value given 
#' a nodes parents take specific values.
#' @param object the node object.
#' @param pvals the values taken by the node's parents.
#' @param met "nat" or "aux": Generate from true or effective parents
#' of noisy or nodes
#' @return the generated value. 
#' @keywords internal
gen <- function (object,pvals,aux,met="aux") {
  UseMethod('gen', object)
}
#' Generate random value for a dirichlet node
#' 
#' Generate a random value for a dirichlet node given the values that the
#' node's parents take.
#' @param dirichlet.object the node.
#' @param pvals vector specifying the values taken by the node's parents.
#' @return the generated value. 
#' @keywords internal
gen.dirichlet <- function (object,pvals,aux,met) {
  row <- calculate.row(pvals,object$pvals)
  out<-genFromRow(object$params[row,])
  return(out)
}
#' Generate random value for a flag node
#' 
#' Generate a random value for a glaf node given the values that the
#' node's parents take.
#' @param object the node.
#' @param pvals vector specifying the values taken by the node's parents.
#' @return the generated value. 
#' @keywords internal
gen.flag <- function (object,pvals,aux,met) {
  ifelse(identical(as.numeric(pvals[1]),as.numeric(object$params[1,1])),2,1)
}
#' Generate random value for a logand node
#' 
#' Generate a random value for a logand node given the values that the
#' node's parents take.
#' @param object the node.
#' @param pvals vector specifying the values taken by the node's parents.
#' @return the generated value. 
#' @keywords internal
gen.logand <- function (object,pvals,aux,met) {
  i <- identical(as.numeric(pvals),c(object$params))
  ifelse(i,2,1)
}


#' Generate random value for a noisy or node
#' 
#' Generate a random value for a noisy or node given the values 
#' that the node's parents take.
#' @param noisyor the node.
#' @param pvals the values taken by the node's parents.
#' @return the generated value. 
#' @keywords internal
gen.noisyor <- function (object,pvals,aux,met) {
  if (identical(met,"nat")) {
    return (gen_raw.noisyor(object,pvals))
  } else {
    return (gen_aux.noisyor(object,aux))    
  }
}
gen_raw.noisyor <- function (object,pvals) {
  if (length(object$values)==2) {
    fail=object$params[1,2]/sum(object$params[1,2:3])
    if (length(object$parents)>0) {
      for (i in 1:length(object$parents)) {
        if (pvals[i]>1){
          row=object$pindices[i]+pvals[i]-2
          fail=fail*object$params[row,2]/sum(object$params[row,2:3])
        }
      }          
    }
    p=runif(1)
    if (p<fail) {
      return (1)
    }
    return (2)
  } else {
    stop("Raw value generation not permitted for multivariate noisy or nodes.")    
  } 
}
gen_aux.noisyor <- function (object,aux) {
  row <- calculate.row(aux[[object$name]],
    rep(length(object$values),length(aux[[object$name]])))
  if (length(object$values)>2) {
    return (genFromRow(object$params2[row,]))
  } else {
    if (row==1) {
      return (1)
    }
    return (2)
  }
}

#' Generate random value for a logprod node
#' 
#' Generate a random value for a logprod node given the
#' values that the node's parents take.
#' @param object the node.
#' @param pvals vector specifying the values taken by the node's parents.
#' @return the generated value. 
#' @keywords internal
gen.logprod <- function (object,pvals,aux,met) {
  return (calculate.row(pvals,object$pvals))
}
#' Generate random value for a flagprod node
#' 
#' Generate a random value for a flagprod node given the
#' values that the node's parents take.
#' @param object the node.
#' @param pvals vector specifying the values taken by the 
#' node's parents.
#' @return the generated value. 
#' @keywords internal
gen.flagprod <- function (object,pvals,aux,met) {
  a<-ifelse(identical(
    as.numeric(pvals[1]),as.numeric(object$params[1,1])),2,1)
  b<-ifelse(identical(
    as.numeric(pvals[2]),as.numeric(object$params[1,2])),2,1)  
  return (calculate.row(c(a,b),c(2,2)))
}
#' Generate random value for a logor node
#' 
#' Generate a random value for a logor node given the
#' values that the node's parents take.
#' @param object the node.
#' @param pvals vector specifying the values taken by 
#' the node's parents.
#' @return the generated value. 
#' @keywords internal
gen.logor <- function (object,pvals,aux,met) {
  a<-ifelse(identical(
    as.numeric(pvals[1]),as.numeric(object$params[1,1])),2,1)
  b<-ifelse(identical(
    as.numeric(pvals[2]),as.numeric(object$params[1,2])),2,1)  
  return (max(a,b))
}
