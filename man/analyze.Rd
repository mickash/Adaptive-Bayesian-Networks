% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/Analysis.R
\name{analyze}
\alias{analyze}
\title{Analyze network}
\usage{
analyze(analyzer, net, posterior, func = defaultDiagnosticsFunction, ...)
}
\arguments{
\item{analyzer}{An analysis object.}

\item{net}{Network to be analyzed}

\item{posterior}{Posterior probabilities object, either returned from
readResults or (embedded in the network) from infer.bayesnet.}

\item{func}{A function to be handed the results of the
analysis. It will be handed:
func(
   analyzer,
   net,
   posterior,
   defectIndices,      - Indices of defect variables, ordered by probability
   defectNames,        - Name of defect variables, ordered by probability
   defectProbs,        - Probability of defect variables, ordered by probability
   rootCauseIndices,   - Indices of root cause variables, ordered by probability
   rootCauseNames,     - Name of root cause variables, ordered by probability
   rootCauseProbs      - Probability of root cause variables, ordered by probability
   ...
   )
If NULL diagnostics will be printed out to R console.}

\item{...}{Additional arguments to be passed to func}
}
\value{
The output of func
}
\description{
Analyze network
}

