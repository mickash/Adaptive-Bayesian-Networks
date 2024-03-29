% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/AddEdge.R
\name{deepUpdateOnEdgeAdded}
\alias{deepUpdateOnEdgeAdded}
\title{Perform a deep update after adding an edge}
\usage{
deepUpdateOnEdgeAdded(cn, net, from, to, dropImages = F)
}
\arguments{
\item{cn}{An open RODBC connection.}

\item{net}{The network.}

\item{from}{The index of the parent node}

\item{to}{The index of the child node}

\item{dropImages}{Whether images should be dropped or updated.}

\item{zeroIndexed}{Whether the from and to arguments are 0 or 1 indexed.}
}
\value{
The updated network
}
\description{
Update the images and tables of a network after adding an edge.
Note that to be able to update the saved network images, the child must be
a binary noisy or node.
}
\keyword{internal}

