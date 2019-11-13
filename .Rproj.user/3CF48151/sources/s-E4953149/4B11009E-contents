#-------------------------------------
# Primary Function (1): Perform inference in R
#-------------------------------------
#' Perform inference on network
#' 
#' Perform inference on network
#' @param net the network
#' @param evidence vector of integers specifying known values of nodes. This
#' can include NA values for unknown variables. You can also pass a data frame row
#' as the function will cast it to numeric internally.
#' @param num integer specifying the number of samples to used in inference
#' @param update boolean specifying whether to update node parameters
#' @param algorithm A string specifying a valid algorithm. 
#' "IS" = Importance sampling. 
#' "BS" = Backwards sampling
#' "SU" = Selected uniform sampling
#' "MC" = MCMC sampling
#' @param algoSpecific An algorithm specific object:
#' @return updated network with a posterior probabilities object as field
#' posterior
#' @export
#-------------------------------------
predict.bayesnet <- function (
  net,
  evidence,
  num=1000,
  update=FALSE,
  algorithm="IS",
  algoSpecific=NULL
) {
  # Avoid difficulties if they have passed a data frame row
  evidence=as.numeric(evidence)
  
  if (length(evidence)!=length(net$nodes))  stop('Evidence/node length mismatch.')
  
  samples<-performSampling(algorithm,net,evidence,num,algoSpecific)
  samples__<-c()
  lhs<-c()
  for (i in 1:length(samples)) {
    samples__<-rbind(samples__,samples[[i]]$sample)
    lhs<-c(lhs,samples[[i]]$lh)
  }
  
  if (isTRUE(update)) {
    extract.likelihood <-function (sample) {
      return(sample$lh)
    }
    total=sum(sapply(samples,extract.likelihood))
    if (identical(total,0)) {
      warning("Samples impossible!")
    }
    for (i in 1:length(net$nodes)) {
      net$nodes[[i]]<-adapt(net$nodes[[i]],samples=samples,total=total)              
    }
  }
  
  net$posterior <- lapply(net$nodes,calculate.posteriori,samples=samples,evidence=evidence)  
  
  return(net)
}
#-------------------------------------

#-------------------------------------
# Major Subcomponent (1.1): Perform Sampling
#-------------------------------------
#' Perform sampling
#' 
#' Perform sampling
#' @param algo A string specifying a valid algorithm. 
#' "IS" = Importance sampling. 
#' "BS" = Backwards sampling
#' "SU" = Selected uniform sampling
#' "MC" = MCMC sampling
#' @param net the network
#' @param evidence vector of integers specifying known values of nodes. This
#' can include NA values for unknown variables.
#' @param num integer specifying the number of samples to used in inference
#' @param algoSpecific An algorithm specific object:
#' For importance sampling, this should specify "nat" or "aux" depending on
#' whether auxiliary variables should be sampled, or only the natural variables.
#' @return The generated samples
#' @keywords internal
#-------------------------------------
performSampling<-function(
  algo,
  net,
  evidence,
  num,
  algoSpecific=NULL
) { 
  samples<-NULL
  if (identical(algo,"IS")) {
    samples=performSampling_importanceSampling(num,net,evidence,algoSpecific)
  } else if (identical(algo,"BS")) {
    samples=performSampling_backwardsSimulation(num,net,evidence,algoSpecific)    
  } else if (identical(algo,"SU")) {
    samples=performSampling_selectedUniform(num,net,evidence,algoSpecific)    
  } else if (identical(algo,"MC")) {
    samples=performSampling_MCMC(num,net,evidence,algoSpecific)    
  } else {
    stop("Invalid sampling algorithm.")    
  }
  return (samples)
}
#-------------------------------------