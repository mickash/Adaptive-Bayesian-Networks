% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/Analysis.R
\name{defaultDiagnosticsFunction}
\alias{defaultDiagnosticsFunction}
\title{A Default Diagnostics Function}
\usage{
defaultDiagnosticsFunction(analyzer, net, posterior, defectIndices, defectNames,
  defectProbs, rootCauseIndices, rootCauseNames, rootCauseProbs)
}
\arguments{
\item{analyzer}{An analysis object}

\item{net}{The Bayesnet object}

\item{posterior}{A posterior probabilities object}

\item{defectIndices}{Indices of defect variables, ordered by probability}

\item{defectNames}{Name of defect variables, ordered by probability}

\item{defectProbs}{Probability of defect variables, ordered by probability}

\item{rootCauseIndices}{Indices of root cause variables, ordered by probability}

\item{rootCauseNames}{Name of root cause variables, ordered by probability}

\item{rootCauseProbs}{Probability of root cause variables, ordered by probability}

\item{...}{Additional arguments passed to the analyze function}
}
\description{
A Default Diagnostics Function
}

