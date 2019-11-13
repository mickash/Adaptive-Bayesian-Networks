logor <- function (
  index,
  values,
  name=as.character(index),
  parents,
  pvals,
  params,
  params2=NULL) {
  out<-list(
    index=index,
    name=name,
    values=values,
    parents=parents,
    params=params,                                     # Parameters1 
    pvals=pvals,                                       # Parameters2
    params2=params2
  )
  class(out)<-'logor'
  validate(out)
  return (out)  
}
