#' Constructor for logical and node
#' 
#' Constructor for logical and node
#' @param index integer giving the index of the node
#' @param name string giving name of node
#' @param values vector of strings giving names of values of node
#' @param parents vector specifying indices of parents of node
#' @param pvals vector specifying number of values for each parent of node
#' @param params matrix specifying values of parents for logical-and.
#' @return constructed logand node
#' @keywords internal
logand <- function (index,name,values,parents,pvals,params,params2) {
  out<-list(
    index=index,
    name=name,
    values=values,
    parents=parents,
    params=params,  # Parameters1
    pvals=pvals     
  )
  class(out)<-'logand'
  validate(out)
  return (out)
}
#' Test logical and class and methods
#' 
#' Test logical and class and methods
#' @export
#' @keywords internal
test.logand <- function () {
  parents<-c(1,2)
  pvals<-c(2,3)
  tvals<-c(1,2)
  
  # Case: Parents, default params
  node1 <- logand(index=3,name="test",values=c("False","True"),parents=parents,pvals=pvals,params=tvals)
  test.traits(node1,tvals,0,c(),pvals,parents,3,c("False","True"))
  
  cat('Construction tests successful\n')
}