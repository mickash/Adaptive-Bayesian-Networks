getStraightConditionalDistributions<-function(net,evidence) {
  out<-list()
  for (i in net$nodes) {
    out[[i$index]]=getStraightConditionalDistribution(i,evidence)
  }
  return(out)
}
getStraightConditionalDistribution<-function(node,evidence) {
  if (!identical(class(node),"noisyor")) {
    return (NULL)
  }
  out<-list()
  if (!is.na(evidence[node$index])) {
    out[[evidence[node$index]]]<-
      getStraightConditionalDistribution__(node,evidence[node$index])
  } else {
    for (i in 1:length(node$values)) {
      out[[i]]<-
        getStraightConditionalDistribution__(node,i)
    }
  }
  return(out)
}
getStraightConditionalDistribution__<-function(node,value) {
  if (length(node$parents)==0) {
    return (prob(node,c(),value,NULL,"nat"))
  } 
  pvals<-rep(1,length(node$parents))
  out<-c()
  cont<-T
  while (cont) {
    cont=prod(pvals)!=prod(node$pvals)
    out<-append(out,prob(node,pvals,value,NULL,"nat"))
    for (i in length(node$parents):1) {
      if (pvals[i]==node$pvals[i]) {
        pvals[i]<-1
      } else {
        pvals[i]<-pvals[i]+1
        break
      }
    }
  }
  return (out)
}