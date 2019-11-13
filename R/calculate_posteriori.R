#' Calcalate posteriori distribution for node
#' 
#' Calcalate posteriori distribution for node given samples and evidence
#' @param node the node
#' @param samples list of samples and their likelihood given evidence
#' @param evidence vector of integers specifying known values of nodes. This
#' can include NA values for unknown variables.
#' @return posteriori distribution for node
#' @keywords internal
calculate.posteriori<-function(node,samples,evidence) {
  temp <- rep(0,length(node$values))
  if (!is.na(evidence[node$index])) {
    temp[evidence[node$index]]=1
    return(temp)
  }
  for (i in 1:length(samples)) {
    v<-samples[[i]]$sample[node$index]
    temp[v]=temp[v]+samples[[i]]$lh
  }
  calculate.dprob(1:length(temp),temp)
}
