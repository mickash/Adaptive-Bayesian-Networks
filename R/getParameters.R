#' S3 generic function for getting the parameters from a node.
#' 
#' S3 generic function for getting the parameters from a node.
#' @param object the node object.
#' @return a vector giving the parameters
#' @keywords internal
getParameters1 <-  function (object) {
  UseMethod('getParameters1', object)
}
getParameters1.dirichlet <-  function (object) {
  return (object$params)
}
getParameters1.flag <-  function (object) {
  return (object$params-1)
}
getParameters1.logand <-  function (object) {
  return (object$params-1)
}
getParameters1.noisyor <-  function (object) {
  return (object$params)
}
getParameters1.logprod <-  function (object) {
  return (object$params)
}
getParameters1.flagprod <-  function (object) {
  return (object$params-1)
}
getParameters1.logor <-  function (object) {
  return (object$params-1)
}
#' S3 generic function for getting the parameters from a node.
#' 
#' S3 generic function for getting the parameters from a node.
#' @param object the node object.
#' @return a vector giving the parameters
#' @keywords internal
getParameters2 <-  function (object) {
  UseMethod('getParameters2', object)
}
getParameters2.dirichlet <-  function (object) {
  return (NULL)
}
getParameters2.flag <-  function (object) {
  return (NULL)
}
getParameters2.logand <-  function (object) {
  return (NULL)
}
getParameters2.noisyor <-  function (object) {
  if (is.null(object$pindices)||length(object$pindices)==0) {
    return (NULL)
  }
  return (matrix(object$pindices-1,nrow=1))
}
getParameters2.logprod <-  function (object) {
  return (NULL)
}
getParameters2.flagprod <-  function (object) {
  return (NULL)
}
getParameters2.logor <-  function (object) {
  return (NULL)
}

#' S3 generic function for getting the parameters from a node.
#' 
#' S3 generic function for getting the parameters from a node.
#' @param object the node object.
#' @return a vector giving the parameters
#' @keywords internal
getParameters3 <-  function (object) {
  return (object$params2) # YES - THIS SHOULD BE PARAMS2 - Parameters2 are the pindices
}

#' S3 generic function for getting the parameters from a node.
#' 
#' S3 generic function for getting the parameters from a node.
#' @param object the node object.
#' @return a vector giving the parameters
#' @keywords internal
getMiscString <-  function (object) {
  UseMethod('getMiscString', object)
}
getMiscString.dirichlet <-  function (object) {
  return ("NULL")
}
getMiscString.flag <-  function (object) {
  return ("NULL")
}
getMiscString.logand <-  function (object) {
  return ("NULL")
}
getMiscString.noisyor <-  function (object) {
  return (paste("'",object$updateDisambiguation,"'",sep=""))
}
getMiscString.logprod <-  function (object) {
  return ("NULL")
}
getMiscString.flagprod <-  function (object) {
  return ("NULL")
}
getMiscString.logor <-  function (object) {
  return ("NULL")
}

