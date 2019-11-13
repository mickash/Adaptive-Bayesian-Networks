#' Constructor for flag node
#' 
#' Constructor for flag node
#' @param index integer giving the index of the node
#' @param name string giving name of node
#' @param values vector of strings giving names of values of node. 
#' This must be length 2.
#' @param pvals vector specifying number of values for each parent of node. 
#' This must be length 1
#' @param params matrix specifying parameters of node: The relevant value of the
#' parent as a 1x1 matrix. 
#' @return constructed flag node
#' @keywords internal
flag <- function (index,name,values,parents,pvals,params,params2) {

  out<-list(
    index=index,
    name=name,
    values=values,
    parents=parents,
    params=params,  # Parameters1
    pvals=pvals     
  )
  class(out)<-'flag'
  validate(out)
  return (out)
}
#' Test flag class and methods
#' 
#' Test flag class and methods
#' @export
#' @keywords internal
test.flag <- function () {
  test.prob.flag <- function (node,pvals,value,expected) {
    if (prob.flag(node,pvals,value)!=expected) {
      stop('Flag probability calculation error.')
    }
  }
  
  parents<-c(1)
  pvals<-c(3)
  tvals<-c(2)
  vals<-c('False','True')
  
  # Case: Parents, default params
  node1 <- flag(index=3,name='tester',vals,parents=parents,pvals=pvals,params=matrix(tvals))
  test.traits(node1,matrix(tvals),0,c(),pvals,parents,3,vals)
  
  cat('Construction tests successful\n')
}