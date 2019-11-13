#' Get the edges present in a network
#' 
#' Get the edges present in a network
#' @param net the network
#' @return a vector specifying the edges present, with odd indexed values giving 
#' the parent indices, and even number indices giving the child index. 
#' @export
getEdges <- function (net) {
  edges <-  function (object) {
    return (c(rbind(object$parents, rep(object$index,length(object$parents))))) 
  }
  non.flat<-lapply(net$nodes,edges)
  unlist(non.flat)
}
