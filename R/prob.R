#' Get probability of value given parents
#' 
#' S3 generic function for getting the probability of a value given 
#' a nodes parents take specific values.
#' @param object the node object.
#' @param pvals the values taken by the node's parents.
#' @param val the value taken by the node.
#' @return the probability of the node taking the specified value given its 
#' parents take the specified values. 
#' @keywords internal
prob <- function (object,pvals,val,aux,met="aux") {
  UseMethod('prob', object)
}
#' Calculate probability of value given parents for dirichlet node
#' 
#' Calculate probability of value given parents for dirichlet node
#' @param dirichlet.object the node
#' @param pvals vector specifying values taken by parents
#' @param val integer specifying value taken by node
#' @return numeric specifying probability of value given parents for node
#' @keywords internal
prob.dirichlet <- function (object,pvals,val,aux,met) {
  row <- calculate.row(pvals,object$pvals)
  calculate.dprob(val,object$params[row,])
}
#' Calculate probability of value given parents for flag node
#' 
#' Calculate probability of value given parents for flag node
#' @param object the node
#' @param pvals vector specifying values taken by parents
#' @param val integer specifying value taken by node
#' @return numeric specifying probability of value given parents for node
#' @keywords internal
prob.flag <- function (object,pvals,val,aux,met) {
  ifelse(identical(gen(object,pvals),val),1,0)
}
#' Calculate probability of value given parents for logical and node
#' 
#' Calculate probability of value given parents for logical and node
#' @param object the node
#' @param pvals vector specifying values taken by parents
#' @param val integer specifying value taken by node
#' @return numeric specifying probability of value given parents for node
#' @keywords internal
prob.logand <- function (object,pvals,val,aux,met) {
  ifelse(identical(gen(object,pvals),val),1,0)
}
#' Get probability noisy or node takes specified value given parents
#' 
#' Get probability of a noisy or node take specified value given parents
#' take specified values. Overrides generic s3 method.
#' @param object the node object.
#' @param current.pvals the values taken by the node's parents.
#' @param val the value taken by the node.
#' @return the probability of the node taking the specified value given its 
#' parents take the specified values. 
#' @keywords internal
prob.noisyor<-function(object,pvals,val,aux,met) {
  if (identical(met,"nat")) {
    return (prob_nat.noisyor(object,pvals,val))
  } else {
    return (prob_aux.noisyor(object,pvals,val,aux))
  }
}
prob_nat.noisyor<-function(node,pvals,val) {
  if (length(node$values)==2) {
    fail=node$params[1,2]/sum(node$params[1,2:3])    
    if (length(pvals)>0) {
      for (i in 1:length(pvals)) {
        if (pvals[i]>1) {
          row=node$pindices[i]+pvals[i]-2
          fail=fail*node$params[row,2]/sum(node$params[row,2:3])
        }
      }        
    }
    if (val==1) {
      return (fail)
    } else {
      return (1-fail)
    }    
  }
  else {
    col<-node$params2[,val]
    repeats=1
    for (i in 0:length(node$parents)) {
      probs=getParameter2Row(node,i,pvals)
      probs=probs/sum(probs)
      repLength=length(col)/repeats
      valueLength=repLength/length(node$values)
      for (j in 1:repeats) {
        for (k in 1:length(node$values)) {
          from=(j-1)*repLength+(k-1)*valueLength+1
          to=(j-1)*repLength+k*valueLength
          col[from:to]=col[from:to]*probs[k]
        }
      }
      repeats=repeats*length(node$values)
    }
    row=genFromRow(col)
    return (genFromRow(node$params2[row,]))
  }
}
prob_aux.noisyor<-function(node,pvals,val,aux) {
  if (length(node$values)==2) {
    if (sum(aux[[node$name]])>length(aux[[node$name]])) {
      if (val==2) {
        out=1
      } else {
        out=0
      }
    } else {
      if (val==2) {
        out=0
      } else {
        out=1
      }
    }
  } else {
    row <- calculate.row(aux[[node$name]],
              rep(length(node$values),length(aux[[node$name]])))
    out= calculate.dprob(val,node$params2[row,])
  }
  return (out)
}

#' Calculate probability of value given parents for 
#' logical product node
#' 
#' Calculate probability of value given parents for 
#' logical product node
#' @param object the node
#' @param pvals vector specifying values taken by parents
#' @param val integer specifying value taken by node
#' @param aux auxiliary samples
#' @return numeric specifying probability of value given parents for node
#' @keywords internal
prob.logprod <- function (object,pvals,val,aux,met) {
  ifelse(
    identical(val,calculate.row(pvals,object$pvals)),1,0)
}
#' Calculate probability of value given parents for 
#' flag product node
#' 
#' Calculate probability of value given parents for 
#' flag product node
#' @param object the node
#' @param pvals vector specifying values taken by parents
#' @param val integer specifying value taken by node
#' @param aux auxiliary samples
#' @return numeric specifying probability of value given parents for node
#' @keywords internal
prob.flagprod <- function (object,pvals,val,aux,met) {
  a<-ifelse(identical(
    as.numeric(pvals[1]),as.numeric(object$params[1,1])),2,1)
  b<-ifelse(identical(
    as.numeric(pvals[2]),as.numeric(object$params[1,2])),2,1)  
  ifelse(
    identical(val,calculate.row(c(a,b),c(2,2))),1,0)
}
#' Calculate probability of value given parents for 
#' logical or node
#' 
#' Calculate probability of value given parents for 
#' logical or node
#' @param object the node
#' @param pvals vector specifying values taken by parents
#' @param val integer specifying value taken by node
#' @param aux auxiliary samples
#' @return numeric specifying probability of value given parents for node
#' @keywords internal
prob.logor <- function (object,pvals,val,aux,met) {
  a<-ifelse(identical(
    as.numeric(pvals[1]),as.numeric(object$params[1,1])),2,1)
  b<-ifelse(identical(
    as.numeric(pvals[2]),as.numeric(object$params[1,2])),2,1)  
  ifelse(identical(val,max(a,b)),1,0)
}
