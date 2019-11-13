 #' Noisy Or constructor
#'  
#'  Constructs a noisy or node
#'  @param index an integer giving the index of the node
#'  @param values a vector specifying the values of the node, as strings
#'  @param name a string specifying the name of the node
#'  @param parents a vector of integers specifying the indices of the parents of the node
#'  @param pvals a vector of integers specifying the number of values each parent has
#'  @param params a matrix specifying the parameters of the node
#'  @return a new noisy or node
#' @keywords internal
noisyor <- function (
  index,
  values,
  name=as.character(index),
  parents,
  pvals,
  params=NULL,
  params2=NULL,
  updateDisambiguation='T') {

  # Create pindices
  pindices=calculate.pindices.noisyor(pvals)
  
  # Create hyperparameters if missing
  if (is.null(params)) {
    eff.pvals <- pvals-1
    param.number <- length(values)+sum(eff.pvals)*(length(values))
    params<-matrix(rep(1,param.number),ncol=(length(values)))
  }
  
  # Create (effective) parent indices as first colomn of parameters if missing
  if (ncol(params)==length(values)) {
    col=rep(0,nrow(params))
    temp=0
    for (i in 1:nrow(params)) {
      if (i==1) {
        col[i]=0
      } else if (i==2) {
        col[i]=1
        temp=1
      } else {
        if (temp>length(pindices) || i<pindices[temp]) {
          col[i]=temp
        } else {
          temp=temp+1
          col[i]=temp
        }
      }
    }
    params <- (cbind(col,params))      
  }
  
  if (length(values)!=2) {
    # Multinomial Noisy Or
    if (is.null(params2)) {
      params2<-makeWeightedMutex(length(parents)+1,length(values))
    }
    if (is.null(updateDisambiguation)) {
      updateDisambiguation='F'
    }    
  } else {
    updateDisambiguation='F'
    if (!is.null(params2)) {
      stop ('Binary noisy-or distributions should not include a disambiguation matrix.')      
    }
  }

  out<-list(
    name=name,
    index=index,
    parents=parents,
    values=values,
    params=params,                               # Parameters1
    pindices=pindices,  # Parameters2
    pvals=pvals,                                 
    params2=params2,
    updateDisambiguation=updateDisambiguation
  )
  class(out)<-'noisyor'
  validate(out)
  return (out)
}

update <- function (vec,max) {
  update.inner <- function (v,index) {
    if (v[index]==max) {
      v[index]=1      
      if (index==1) {
        stop('Completed truth table.')
      }
      return (update.inner(v,index-1))
    } else {
      v[index]=v[index]+1
      return (v)
    }
  }
  update.inner(vec,length(vec))
}
#' Make a mutual exclusion disambiguation matrix
#' 
#' Make a mutual exclusion disambiguation matrix
#' @param parents A scalar specifying how many parents the node has.
#' @param vals A scalar specifying how many values the node has.
#' @export
makeMutex <- function (parents,vals) { 
  m<-matrix(rep(0,vals*(vals^parents)),ncol=vals)
  vec <- rep(1,parents);
  for (i in 1:vals^parents) {
    on<-vec[vec!=1];
    if (length(on)==0) {
      m[i,1]<-1
    } else {
      m[i,on]=1
    } 
    vec<-update(vec,vals)  
  }
  return (m)
}
#' Make a weighted mutual exclusion disambiguation matrix
#' 
#' Make a weighted mutual exclusion disambiguation matrix
#' @param parents A scalar specifying how many parents the node has.
#' @param vals A scalar specifying how many values the node has.
#' @export
makeWeightedMutex <- function (parents,vals) { 
  m<-matrix(rep(0,vals*(vals^parents)),ncol=vals)
  vec <- rep(1,parents);
  for (i in 1:vals^parents) {
    on<-vec[vec!=1];
    if (length(on)==0) {
      m[i,1]<-1
    } else {
      for (j in 1:length(on)) {
        m[i,on[j]]=m[i,on[j]]+1
      }
    } 
    if (i!=vals^parents) {
      vec<-update(vec,vals)        
    }
  }
  return (m)
}


#' Calculate parameter jump table for noisy or node
#' 
#' Calculate jump table specifying the first row in the node's parameters 
#' corresponding to each parent for a noisy or node
#' @param pvals a vector specifying the number of values each parent has.
#' @return a vector specifying the first row in the node's parameters 
#' corresponding to each parent
#' @keywords internal
calculate.pindices.noisyor <- function (pvals) {   
  if (is.null(pvals)) {
    return (NULL)
  }
  if (length(pvals)==1) {
    return (c(2))
  }
  out<-rep(1,length(pvals))
  v<-pvals-1
  out[1]<-2
  for (i in 2:length(pvals)) {
    out[i]=out[i-1]+v[i-1]  
  }
  return (out)
}

#' Test noisy or node class and methods
#' 
#' Test noisy or node class and methods
#' @export
#' @keywords internal
test.noisyor <- function () {
  parents<-c(1,2,3)
  pvals<-c(2,3,4)
  custom.params=matrix(c(9,1,1,9,1,4,2,3,1,2,1,2,1,3),byrow=TRUE,nrow=7)
  vls<-c('False','True')
  
  # Case: No parents, default params
  node1 <- noisyor(index=1,values=vls)
  test.traits(node1,matrix(c(1,1),nrow=1),1,c(),c(),c(),1,vls)
  
  # Case: Parents, default params
  node2 <- noisyor(index=5,values=vls,parents=parents,pvals=pvals)
  test.traits(node2,matrix(rep(1,14),ncol=2),7,c(2,3,5),pvals,parents,5,vls)
  
  # Case: No parents, params
  node3 <- noisyor(index=5,values=vls,params=matrix(c(89,11),nrow=1))
  test.traits(node3,matrix(c(89,11),ncol=2),1,c(),c(),c(),5,vls)
  
  # Case: parents, params
  node4 <- noisyor(index=5,values=vls,parents=parents,pvals=pvals,params=custom.params)
  test.traits(node4,custom.params,7,c(2,3,5),pvals,parents,5,vls)
  
  cat('Construction tests successful\n')
}