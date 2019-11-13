#' Adapt parameters
#' 
#' S3 generic function for adapting a node's parameters given a set of samples.
#' @param object the node object.
#' @param samples the set of samples.
#' @param total the sum of the likelihood of the samples.
#' @return the updated node
#' @keywords internal
adapt <- function (object,samples,total) {
  UseMethod('adapt', object)
}
#' Adapt parameters for dirichlet node
#' 
#' AdaptS a dirichlet node's parameters given a set of samples.
#' @param dirichlet.object the node object.
#' @param samples the set of samples.
#' @param total the sum of the likelihood of the samples.
#' @return the updated node
#' @keywords internal
adapt.dirichlet <- function (object,samples,total) {
  for (i in 1:length(samples)) {
    sample<-samples[[i]]$sample
    lh<-samples[[i]]$lh/total
    pvals<-sample[object$parents]
    row <- calculate.row(pvals,object$pvals)
    object$params[row,sample[object$index]]<-
      object$params[row,sample[object$index]]+lh  
  }
  return (object)
}
#' Adapt parameters for flag node
#' 
#' AdaptS a flag node's parameters given a set of samples.
#' @param object the node object.
#' @param samples the set of samples.
#' @param total the sum of the likelihood of the samples.
#' @return the updated node
#' @keywords internal
adapt.flag <- function (object,samples,total) {
  return(object)
}
#' Adapt parameters for logical and node
#' 
#' AdaptS a logical and node's parameters given a set of samples.
#' @param object the node object.
#' @param samples the set of samples.
#' @param total the sum of the likelihood of the samples.
#' @return the updated node
#' @keywords internal
adapt.logand <- function (object,samples,total) {
  return(object)
}
#' Adapt parameters of noisy or node
#'
#' Adapt parameters of noisy or node given current samples
#' @param noisyor.object the node
#' @param samples list of current samples
#' @param total numeric giving sum of likelihood of all samples
#' @param debug boolean specifying if debug messages should be shown
#' @return updated node
#' @keywords internal
adapt.noisyor <- function (object,samples,total) {
  
  for (i in 1:length(samples)) {
    
    sample<-samples[[i]]$sample
    aux<-samples[[i]]$auxiliarySamples[[object$name]]
    lh<-samples[[i]]$lh/total
    
    # Slack
    # plus 1 because of eff. parents column
    object$params[1,aux[1]+1]=
      object$params[1,aux[1]+1]+lh

    # Other parents
    if (length(object$parents)>0) {
      for (i in 1:length(object$parents)) {
        if (sample[object$parents[i]]>1) {
          row<-object$pindices[i]+sample[object$parents[i]]-2
          # plus 1 because of slack
          auxCol<-round(object$params[row,1])+1 
          # plus 1 because of eff. parents column
          object$params[row,aux[auxCol]+1]=
            object$params[row,aux[auxCol]+1]+lh
        }
      }
    }

    val<-sample[object$index]
    row <- calculate.row(aux,
      rep(length(object$values),length(aux)))
    object$params2[row,val]=
      object$params2[row,val]+lh
  }
  
  return (object)
}

#' Adapt parameters for logprod node
#' 
#' AdaptS a logprod node's parameters given a set of samples.
#' @param object the node object.
#' @param samples the set of samples.
#' @param total the sum of the likelihood of the samples.
#' @return the updated node
#' @keywords internal
adapt.logprod <- function (object,samples,total) {
  return(object)
}
#' Adapt parameters for flagprod node
#' 
#' AdaptS a flagprod node's parameters given a set of samples.
#' @param object the node object.
#' @param samples the set of samples.
#' @param total the sum of the likelihood of the samples.
#' @return the updated node
#' @keywords internal
adapt.flagprod <- function (object,samples,total) {
  return(object)
}
#' Adapt parameters for logical or node
#' 
#' AdaptS a logical or node's parameters given a set of samples.
#' @param object the node object.
#' @param samples the set of samples.
#' @param total the sum of the likelihood of the samples.
#' @return the updated node
#' @keywords internal
adapt.logor <- function (object,samples,total) {
  return(object)
}
