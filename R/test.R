#' Test package
#' 
#' Test package
#' @keywords internal
#' @export
test.abn <- function () {
  test.noisyor()
  test.dirichlet()
  test.flag()
  test.logand()
  test.bayesnet()  
}
#' Test a constructed node has the expected traits
#' 
#' Test a constructed node has the expected traits
#' @param node the constructed node.
#' @param params a matrix giving the expected parameters for the node.
#' @param degree a numeric giving the expected degrees of freedom of the node
#' @param pindices.jumptable a vector giving the expected parent indices, 
#' jump table or pmap of the mode, depending on node type.
#' @param parents a vector giving the indices of the parents of the node
#' @param pvals a vector giving the expected number of values for each parent of the node
#' @param index an integer giving the expected index of the node
#' @param vals a vector giving the expected values of the node
#' @return vector of boolean values indicating whether above match actual traits
#' @keywords internal
test.traits <- function (node,
                         params,
                         degree,
                         pindices.jumptable,
                         pvals,
                         parents,
                         index,
                         vals) {
  a<-identical(params,node$params)
  b<-identical(degree,node$degree) 
  # Note logand compares with node$jumpTable which is NULL
  c<-TRUE
  if (class(node)=='noisyor') {
    c<-identical(pindices.jumptable,node$pindices)
  } else if (class(node)=='mutex') {
    c<-identical(pindices.jumptable,node$pmap)    
  } else {
    c<-identical(pindices.jumptable,node$jumpTable)
  }
  d<-identical(pvals,node$pvals) 
  e<-identical(parents,node$parents) 
  f<-identical(index,node$index)
  g<-identical(vals,node$values)
  v<-c(a,b,c,d,e,f,g)
  if (!all(v)) {
    print(node)
    print(v)
    stop('Node traits mismatch.')
  }
}