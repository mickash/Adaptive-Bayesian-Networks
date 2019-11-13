performSampling_MCMC<-function(num,net,evidence,algoSpecific){
  burn<-algoSpecific$burn
  thinner<-algoSpecific$thinner
  children<-calculateChildren(net)
  samples<-list()
  sample<-createInitialMCMCsample(net,evidence)
  for (i in 1:burn) {
    sample<-getMCMCSample(net,children,sample,evidence,F)    
  }
  for (i in 1:num) {
    for (j in 1:thinner) {
      sample<-getMCMCSample(net,children,sample,evidence,j==thinner)          
    }
    samples[[length(samples)+1]]<-sample
  }
  return (samples)
}
getMCMCSample<-function(net,children,s,evidence,getAux){
  for (node in net$nodes) {
    if (is.na(evidence[node$index])) {
      s$sample[node$index]=gen_MB(node,children,net,s$sample,NULL,"nat")      
    }
    if (getAux) {
      s$auxiliarySamples=
        createAuxiliarySamples(node,s$sample,s$auxiliarySamples,
                               s$sample[node$index],"MB")      
    }
  }
  return (s)
}
createInitialMCMCsample<-function(net,evidence){
  sample<-evidence
  for (node in net$nodes) {
    if (is.na(evidence[node$index])) {
      sample[node$index]=sample(length(node$values),1)
    }
  }
  # Don't worry about auxiliary samples here
  list(
    sample=sample,
    lh=1,
    auxiliarySamples=list())
}

