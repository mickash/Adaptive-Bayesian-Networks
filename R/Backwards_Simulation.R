# Backward simulation
#' @keywords internal
performSampling_backwardsSimulation<-function(
  num,
  net,
  evidence,
  met="aux"
) {
  bOrder<-getBackwardsOrder_backwardSimulation(evidence,net)
  fOrder<-getForwardOrder_backwardSimulation(evidence,net,bOrder)
  if (identical(met,"nat")) {
    cds<-getStraightConditionalDistributions(net,evidence)
    return (lapply(1:num,getBackwardSimulationSample_nat,evidence,
                   net,bOrder,fOrder,cds))
  } else {
    return (lapply(1:num,getBackwardSimulationSample_aux,evidence,
                   net,bOrder,fOrder))    
  }
}
# Backward simulation
#' @keywords internal
getBackwardSimulationSample_nat<-function(
  n,
  evidence,
  net,
  bOrder,
  fOrder,
  cds
) {  
  # 2. Perform sampling.
  s<-list(
    sample=evidence,
    lh=1,
    auxiliarySamples=list()
  )
  for (i in bOrder) {
    s<-backwardsSample(net$nodes[[i]],s,cds,"nat")
  }
  for (i in fOrder) {
    s<-forwardsSample(net$nodes[[i]],s,"nat")
  }
  
  # Return sample, auxiliary samples and likelihood
  return (s)
}
#' @keywords internal
getBackwardSimulationSample_aux<-function(
  n,
  evidence,
  net,
  bOrder,
  fOrder
) {  
  # 2. Perform sampling.
  # Likelihood adjusted in sampling
  s<-list(
    sample=evidence,
    lh=1,
    auxiliarySamples=list()
  )
  for (i in bOrder) {
    s<-backwardsSample(net$nodes[[i]],s,NULL,"aux")
  }
  for (i in fOrder) {
    s<-forwardsSample(net$nodes[[i]],s,"aux")
  }
  
  # Return sample, auxiliary samples and likelihood
  return (s)
}

#' @keywords internal
getForwardOrder_backwardSimulation<-function(
  evidence,
  net,
  bOrder
) {
  # Forward simulation order
  fOrder<-c()
  # Find remaining nodes.
  remaining<-setdiff((1:length(evidence)),bOrder)
  while (length(remaining)>0) {
    nextGroup<-getRelativeOrphans(remaining,net)
    fOrder=append(fOrder,nextGroup)
    remaining=setdiff(remaining,nextGroup)
  }
  return (fOrder)
}

#' @keywords internal
getBackwardsOrder_backwardSimulation<-function(
  evidence,
  net
) { 
  # Calculate backward simulation order
  bOrder<-c()
  
  # Find each evidence node, mark as enodes.
  nextNodes<-which(!is.na(evidence))
  while (length(nextNodes)>0) {
    bOrder=append(bOrder,nextNodes)      
    # To take a random order, use:
    # Take random order of nextNodes, place in order.
    # Note sample function with single value, n, in nextNodes samples from 1:n
    #    if (length(nextNodes)==1) {
    #      bOrder=append(bOrder,nextNodes)      
    #    } else {
    #      bOrder=append(bOrder,sample(nextNodes,length(nextNodes)))      
    #    }
    # In this order, look at enodes to find possible backward samples.
    # New backward sampled nodes with parents set to be enodes.    
    nextNodes<-getBackwardSamplees(nextNodes,bOrder,net)
  }
  # bOrder now contains the nodes to be sampled backwards, including 
  # orphan backward sampled nodes that only adjust sample likelihood.
  return (bOrder)
}

# Find parents of nodes in nodes that are not in ignore.
#' @keywords internal
getBackwardSamplees<-function(nodes,ignore,net){
  parents<-c()
  for (i in nodes) {
    parents<-union(parents,net$nodes[[i]]$parents)
  }
  setdiff(parents,ignore)
}

# Find nodes in nodes that do not have parents in nodes.
#' @keywords internal
getRelativeOrphans<-function(nodes,net) {
  out<-c()
  for (i in nodes) {
    if (!any(net$nodes[[i]]$parents%in%nodes)) {
      out<-append(out,i)
    }
  }
  return (out)
}
