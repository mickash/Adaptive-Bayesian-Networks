#' Plot a network
#' 
#' Plot a network. If the network has a $layout matrix, this will be used in the
#' plotting, otherwise the plot will be random. The layout matrix is that used
#' by igraph and should have 
#' two columns specifying the x and y coordinates of the nodes.
#' @param x the network to plot
#' @param ... ignored
#' @export
plot.bayesnet <- function (x,...) {
  edges <- getEdges(x)
  gr<-igraph::graph(edges,length(x$nodes))
  igraph::plot.igraph(gr,layout=x$layout)
}
