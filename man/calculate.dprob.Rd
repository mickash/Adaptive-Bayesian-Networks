% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/prob_functions.R
\name{calculate.dprob}
\alias{calculate.dprob}
\title{Calculate dirichlet probability}
\usage{
calculate.dprob(i, v)
}
\arguments{
\item{i}{index of value}

\item{v}{vector of values}
}
\value{
v[i]/sum(v)
}
\description{
Calculates the proportion of a particular entry in a vector to the sum of all
entries. Use in calculating MAP values of probability parameters in multinomial
dirstributions with dirichlet hyperparameters.
}
\keyword{internal}

