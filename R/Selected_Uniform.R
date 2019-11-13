
#' @keywords internal
performSampling_selectedUniform<-function(num,net,evidence,algo){
  met<-algo$method
  if (is.null(met)) {
    met="aux"
  }
  selected<-algo$selected
  torder<-calculate.torder(net)
  lapply(1:num,getSelectedUniformSample,net,evidence,torder,selected,met)
}

#' @keywords internal
getSelectedUniformSample<-function(
  n,
  net,
  evidence,
  torder,
  selected,
  met
) { 
  s<-list(
    sample = evidence,      
    auxiliarySamples=list(),
    lh=1,
    debugA=c(1),
    debugB<-c()
  )
  for (i in torder) {    
    if (i%in%selected) {
      s<-uniformSample(net$nodes[[i]],s,met)
      p<-s$lh/s$debugA[length(s$debugA)]
      s$debugB<-append(s$debugB,p)
      s$debugA<-append(s$debugA,s$lh)
    } else {
      s<-forwardsSample(net$nodes[[i]],s,met)      
      p<-s$lh/s$debugA[length(s$debugA)]
      s$debugB<-append(s$debugB,p)
      s$debugA<-append(s$debugA,s$lh)
    }
  }  
  return (s)
}
