% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/backward_sampling.R
\name{backwardsSample}
\alias{backwardsSample}
\title{Generate backward sample for node}
\usage{
backwardsSample(node, s, straightCDS = NULL, met = "aux")
}
\arguments{
\item{node}{The node object.}

\item{s}{Sample object, including sample, auxiliary
samples and likelihood (lh).}
}
\value{
The updated sample object.
}
\description{
S3 generic function for generate backward sample for node.
Alters sample, auxiliary samples and likelihood of sample.
}
\keyword{internal}

