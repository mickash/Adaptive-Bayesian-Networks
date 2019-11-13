# Importance Sampling
#'
#' Methods for importance sampling are: nat - sample nodes before
#' auxiliary samples; or aux - sample auxiliary samples before 
#' nodes.
#' @keywords internal
performSampling_importanceSampling<-function(
  num,
  net,
  evidence,
  met="nat"
) { 
  torder<-calculate.torder(net)
  lapply(1:num,getImportanceSamplingSample,net,evidence,torder,met)
}

#' @keywords internal
getImportanceSamplingSample<-function(
  n,
  net,
  evidence,
  torder,
  met
) { 
  # Set up sampling object
  s<-list(
    sample = evidence,      
    auxiliarySamples=list(),
    lh=1
  )

  # Perform Sampling
  for (i in 1:length(torder)) s<-forwardsSample(net$nodes[[torder[i]]],s,met)
  
  # Return sampling object
  return (s)
}

#' @keywords internal
forwardsSample<-function(node,s,met="nat") {
  if (identical(met,"nat")) {
    return(forwardsSample_nat(node,s))
  } else {
    return(forwardsSample_aux(node,s))    
  }
}
#' @keywords internal
forwardsSample_nat<-function(node,s) {
  if (is.na(s$sample[node$index])) {
    s$sample[node$index]<-as.numeric(
      gen(node,s$sample[node$parents],s$auxiliarySamples,"nat")
    )        
  } else {
    s$lh <- s$lh * 
      prob(node,s$sample[node$parents],s$sample[node$index],
           s$auxiliarySamples,met="nat")
  }
  s$auxiliarySamples<-createAuxiliarySamples(
    node,
    s$sample,
    s$auxiliarySamples,      
    s$sample[node$index],
    "MB")
  return (s)
}
#' @keywords internal
forwardsSample_aux<-function(node,s) {
  s$auxiliarySamples<-createAuxiliarySamples(
    node,
    s$sample,
    s$auxiliarySamples,      
    s$sample[node$index],
    met="CD"
  )  
  if (is.na(s$sample[node$index])) {
    s$sample[node$index]<-as.numeric(
      gen(node,s$sample[node$parents],s$auxiliarySamples,"aux")
    )        
  } else {
    s$lh <- s$lh * 
      prob(node,s$sample[node$parents],s$sample[node$index],
           s$auxiliarySamples,met="aux")
  }
  return (s)
}
