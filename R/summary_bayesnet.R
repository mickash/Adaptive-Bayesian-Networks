#' Get summary of network
#' 
#' Get summary of network, including:
#'  class
#'  size (number of nodes)
#'  list of names of nodes
#'  list of posteriori distributions for nodes
#' Note additional summary arguments ignored.
#' @param object The network
#' @param ... Ignored
#' @export
summary.bayesnet <- function (object,...) {
  name <- function (node) {
    node$name
  }
  names<-unlist(lapply(object$nodes,name))
  cat('Class: bayesnet\nSize: ',length(object$nodes))
  cat('\nNodes: ',names)
  cat('\n')
}