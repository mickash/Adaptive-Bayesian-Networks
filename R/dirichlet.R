#' Dirichlet node constructor
#' 
#' Dirichlet node constructor
#' @param index index of node
#' @param values vector of string giving names of values of node
#' @param name string giving name of node
#' @param parents vector specifying indices of parents of node
#' @param pvals vector specifying number of values for each parent of node
#' @param params matrix specifying parameters of node
#' @return constructed dirichlet node
#' @keywords internal
dirichlet <- function (index,values,name=as.character(index),parents=NULL,pvals=NULL,params=NULL,params2=NULL) {
  vals<-length(values)
  if (is.null(params)) {
    params=matrix(
      rep(1,prod(pvals)*vals),
      ncol=vals)
  }
  out<-list(
    index=index,
    name=name,
    values=values,
    parents=parents,
    params=params,                                    # Parameters1 
    pvals=pvals,                                      
    params2=params2
  )
  class(out)<-'dirichlet'
  validate(out)
  return (out)
}

#' Test dirichlet node class and methods
#' 
#' Test dirichlet node class and methods
#' @export
#' @keywords internal
test.dirichlet <- function () {
  parents<-c(1,2,3)
  pvals<-c(2,3,4)
  custom.params=matrix(
    c(  9,1,  	
        1,1,
        1,1,
        1,1,
        1,1,		#5
        1,2,
        1,9,
        1,4,
        2,3,
        1,1,		#10
        1,1,
        1,1,
        1,1,
        1,2,
        1,2,		#15
        1,9,
        1,4,
        1,1,
        1,1,
        1,1,		#20
        1,1,
        1,1,
        1,1,
        1,2),	byrow=TRUE,ncol=2)
  
  vls<-c('A','B')
  
  # Case: No parents, default params
  node1 <- dirichlet(index=1,values=vls)
  test.traits(node1,matrix(c(1,1),nrow=1),1,c(),c(),c(),1,vls)
  
  # Case: Parents, default params
  node2 <- dirichlet(index=5,values=vls,parents=parents,pvals=pvals)
  test.traits(node2,matrix(rep(1,48),ncol=2),24,c(12,4,1),pvals,parents,5,vls)
  
  # Case: No parents, params
  node3 <- dirichlet(index=5,values=vls,params=matrix(c(89,11),nrow=1))
  test.traits(node3,matrix(c(89,11),ncol=2),1,c(),c(),c(),5,vls)
  
  # Case: parents, params
  node4 <- dirichlet(index=5,values=vls,parents=parents,pvals=pvals,params=custom.params)
  test.traits(node4,custom.params,24,c(12,4,1),pvals,parents,5,vls)
  
  cat('Construction tests successful\n')
}
