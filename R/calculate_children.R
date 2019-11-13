#' @keywords internal
calculateChildren<-function(net) {
  children<-list()
  for (i in net$nodes) {
    kids<-c()
    for (j in net$nodes) {
      if (i$index %in% j$parents) {
        kids<-append(kids,j$index)
      }
    }
    children[[i$name]]=kids
  }
  return (children)
}