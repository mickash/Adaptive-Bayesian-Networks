#' @keywords internal
uniformSample <- function (node,s,met) {
  # s The sample list, including sample and auxiliarySamples as fields
  UseMethod('uniformSample', node)
}
#' @keywords internal
basicUniformSample<-function(node,s) {
  if (is.na(s$sample[node$index])) {
    vals<-length(node$values)
    s$sample[node$index]=sample(vals,1)
    #  s$lh=s$lh*1/vals Not needed as it is simply a constant for all samples    
  }
  return (s)
}
#' @keywords internal
uniformSample.dirichlet<-function(node,s,met) {
  basicUniformSample(node,s)
}  
#' @keywords internal
uniformSample.flag <- function (node,s,met) {
  basicUniformSample(node,s)
}
#' @keywords internal
uniformSample.logand <- function (node,s,met) {
  basicUniformSample(node,s)
}
#' @keywords internal
uniformSample.logprod<-function(node,s,met) {
  basicUniformSample(node,s)
}
#' @keywords internal
uniformSample.flagprod<-function(node,s,met) {
  basicUniformSample(node,s)
}
#' @keywords internal
uniformSample.logor<-function(node,s,met) {
  basicUniformSample(node,s)
}  
#' @keywords internal
uniformSample.noisyor<-function(node,s,met) {
  s<-basicUniformSample(node,s)
  if (identical(met,"aux")) {
    return (backwardsSample.noisyor(node,s))
  } else {
    s$auxiliarySamples=
      createAuxiliarySamples(
        node,
        s$sample,
        s$auxiliarySamples,
        s$sample[node$index],
        met="MB"
        )
    return (s)
  }
}  
