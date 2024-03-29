% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/infer_bayesnet.R
\name{performSampling}
\alias{performSampling}
\title{Perform sampling}
\usage{
performSampling(algo, net, evidence, num, algoSpecific = NULL)
}
\arguments{
\item{algo}{A string specifying a valid algorithm.
"IS" = Importance sampling.
"BS" = Backwards sampling
"SU" = Selected uniform sampling
"MC" = MCMC sampling}

\item{net}{the network}

\item{evidence}{vector of integers specifying known values of nodes. This
can include NA values for unknown variables.}

\item{num}{integer specifying the number of samples to used in inference}

\item{algoSpecific}{An algorithm specific object:
For importance sampling, this should specify "nat" or "aux" depending on
whether auxiliary variables should be sampled, or only the natural variables.}
}
\value{
The generated samples
}
\description{
Perform sampling
}
\keyword{internal}

