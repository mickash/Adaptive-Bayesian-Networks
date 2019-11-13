#-------------------------------------
# Primary Function (1): Create an analysis object
#-------------------------------------
#' Create an analysis object
#' 
#' Create an analysis object. If we have three variables:
#'  Rain     : {None,Light,Heavy}
#'  Sprinker : {Off,On}
#'  WetGrass : {False,True}
#' And we consider wet grass to be a defect, and rain or the
#' sprinkler to be potential root causes, then our matrices 
#' would be:
#'  Root Cause Matrix:
#'    1 , 2
#'    1 , 3
#'    2 , 2
#'  Defect Matrix:
#'    3 , 2
#' @param rootCauses a matrix of two columns specifying the 
#' variables/value combinations that represent root causes.
#' @param defects a matrix of two columns specifying the 
#' variables/value combinations that represent defects.
#' @return An analysis object
#' @export
#-------------------------------------
analyzer <- function (
  rootCauses,
  defects
  ) {
  out<-list (
    rootCauses=rootCauses,
    defects=defects)
  class(out)<-'analyzer'
  return (out)  
}  
#-------------------------------------
# Primary Function (2): Analyze network
#-------------------------------------
#' Analyze network
#' 
#' Analyze network
#' @param analyzer An analysis object.
#' @param net Network to be analyzed 
#' @param posterior Posterior probabilities object, either returned from 
#' readResults or (embedded in the network) from infer.bayesnet. 
#' @param func A function to be handed the results of the
#' analysis. It will be handed:
#' func(
#'    analyzer,
#'    net,
#'    posterior,
#'    defectIndices,      - Indices of defect variables, ordered by probability
#'    defectNames,        - Name of defect variables, ordered by probability
#'    defectProbs,        - Probability of defect variables, ordered by probability
#'    rootCauseIndices,   - Indices of root cause variables, ordered by probability
#'    rootCauseNames,     - Name of root cause variables, ordered by probability
#'    rootCauseProbs      - Probability of root cause variables, ordered by probability
#'    ...
#'    )
#' If NULL diagnostics will be printed out to R console.
#' @param ... Additional arguments to be passed to func
#' @return The output of func
#' @export
#-------------------------------------
analyze <- function (
  analyzer,
  net,
  posterior,
  func=defaultDiagnosticsFunction,
  ...
  ) {
  # Define helper functions
  get.node.value.name<-function(vec,net)
    paste(net$nodes[[vec[1]]]$name,net$nodes[[vec[1]]]$values[vec[2]],sep=":")
  get.node.value.prob.local<-function(vec,posterior) 
    posterior[[vec[1]]][vec[2]]
  
  rootCauseNames<-apply(
    analyzer$rootCauses,
    1,
    get.node.value.name,
    net
  )
  defectNames<-apply(
    analyzer$defects,
    1,
    get.node.value.name,
    net
  )
  rootCauseProbs<-apply(
    analyzer$rootCauses,
    1,
    get.node.value.prob.local,
    posterior
  )
  defectProbs<-apply(
    analyzer$defects,
    1,
    get.node.value.prob.local,
    posterior
  )
  if (length(defectNames)!=length(defectProbs) ||
        length(rootCauseNames)!=length(rootCauseProbs)) {
    stop("Size mismatch in analysis.")
  }
  
  defectOrder<-order(defectProbs,decreasing=T)
  defectProbs<-defectProbs[defectOrder]
  defectNames<-defectNames[defectOrder]
  defectIndices<-analyzer$defects[defectOrder,1]  

  rootCauseOrder<-order(rootCauseProbs,decreasing=T)
  rootCauseProbs<-rootCauseProbs[rootCauseOrder]
  rootCauseNames<-rootCauseNames[rootCauseOrder]
  rootCauseIndices<-analyzer$rootCauses[rootCauseOrder,1]
  
  func(
    analyzer,
    net,
    posterior,
    defectIndices,
    defectNames,
    defectProbs,
    rootCauseIndices,
    rootCauseNames,
    rootCauseProbs,
    ...
  )
}
#-------------------------------------
# Private semi-principle function (3): A Default Diagnostics Function 
#-------------------------------------
#' A Default Diagnostics Function
#' 
#' A Default Diagnostics Function
#' @param analyzer An analysis object
#' @param net The Bayesnet object
#' @param posterior A posterior probabilities object
#' @param defectIndices Indices of defect variables, ordered by probability
#' @param defectNames Name of defect variables, ordered by probability
#' @param defectProbs Probability of defect variables, ordered by probability
#' @param rootCauseIndices Indices of root cause variables, ordered by probability
#' @param rootCauseNames Name of root cause variables, ordered by probability
#' @param rootCauseProbs Probability of root cause variables, ordered by probability
#' @param ... Additional arguments passed to the analyze function
# Note neither exported nor internal: User can read help to understand how it works.
#-------------------------------------
defaultDiagnosticsFunction <- function (
  analyzer,
  net,
  posterior,
  defectIndices,
  defectNames,
  defectProbs,
  rootCauseIndices,
  rootCauseNames,
  rootCauseProbs
  ) { 
  print("Defects: ")
  if (length(defectNames)==0) {
    print("  N/A")
  } else {
    for (i in 1:length(defectNames)) {
      print(paste(defectNames[i],
                  defectProbs[i],sep=' - '))
    }
  }
  print("Root Causes: ")
  if (length(rootCauseNames)==0) {
    print("  N/A")
  } else {
    for (i in 1:length(rootCauseNames)) {
      print(paste(rootCauseNames[i],
                  rootCauseProbs[i],sep=' - '))
    }
  }
}
#-------------------------------------
