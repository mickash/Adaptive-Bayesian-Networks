#' S3 generic function for serialization
#' 
#' S3 generic function for getting the number of additional
#' sample slots required by a node
#' parameters from a node.
#' @param node The node object.
#' @return A vector giving the parameters
#' @keywords internal
getRequiredAuxiliaryPlaces<-function(node) {
  UseMethod('getRequiredAuxiliaryPlaces', node)
}
getRequiredAuxiliaryPlaces.dirichlet<-function(node){
  return (0)
}
getRequiredAuxiliaryPlaces.flag<-function(node){
  return (0)
}
getRequiredAuxiliaryPlaces.logand<-function(node){
  return (0)
}
getRequiredAuxiliaryPlaces.noisyor<-function(node){
  return (round(node$params[nrow(node$params),1])+1)
}
getRequiredAuxiliaryPlaces.logprod<-function(node){
  return (0)
}
getRequiredAuxiliaryPlaces.flagprod<-function(node){
  return (0)
}
getRequiredAuxiliaryPlaces.logor<-function(node){
  return (0)
}
