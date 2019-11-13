createAuxiliarySamples <- function (node,sample,aux,val,met="") {
  UseMethod('createAuxiliarySamples', node)
}
createAuxiliarySamples.dirichlet <- function (node,sample,aux,val,met) {
  return (aux)
}
createAuxiliarySamples.flag <- function (node,sample,aux,val,met) {
  return (aux)
}
createAuxiliarySamples.logand <- function (node,sample,aux,val,met) {
  return (aux)
}
createAuxiliarySamples.noisyor <- function (node,sample,aux,val,met) {
  if (identical(met,"MB")) {
    aux[[node$name]]=createAuxiliarySamples_MB.noisyor(node,sample,aux)
  } else {
    aux[[node$name]]=createAuxiliarySamples_CD.noisyor(node,sample,aux)   
  }
  return (aux)
}
createAuxiliarySamples.logprod <- function (node,sample,aux,val,met) {
  return (aux)
}
createAuxiliarySamples.flagprod <- function (node,sample,aux,val,met) {
  return (aux)
}
createAuxiliarySamples.logor <- function (node,sample,aux,val,met) {
  return (aux)
}
createAuxiliarySamples_CD.noisyor<-function(node,sample,aux) {   
  Out<-rep(1,round(node$params[nrow(node$params),1]+1))
  # Generate Slack Value
  Out[1] = genFromRow(node$params[1,2:ncol(node$params)])
  # Generate Other Values
  if (length(node$parents)!=0) {
    for (i in 1:length(node$parents)) {
      if (sample[node$parents[i]] != 1) {
        # Find Row
        row = node$pindices[i] + sample[1,node$parents[i]] - 2;
        # Generate random
        Out[round(node$params[row, 1])+1] = 
          genFromRow(node$params[row,2:ncol(node$params)])
      }
    }    
  }
  return (Out)    
}
createAuxiliarySamples_MB.noisyor<-function(node,sample,aux) {
  nval<-sample[node$index]
  pvals<-sample[node$parents]
  if (nval==1) {
    return (rep(1,length(node$parents)+1))
  } else {
    possibleEffVals<-rep(length(node$values),length(node$parents)+1)
    
    col=NULL
    if (length(node$values)==2) {
      col=rep(1,prod(possibleEffVals))
      col[1]=0
    } else {
      col=node$params2[,nval]
    }
    
    repeats=1
    for (i in 0:length(node$parents)) {
      probs=getParameter2Row(node,i,pvals)
      probs=probs/sum(probs)
      repLength=length(col)/repeats
      valueLength=repLength/length(node$values)
      for (j in 1:repeats) {
        for (k in 1:length(node$values)) {
          from=(j-1)*repLength+(k-1)*valueLength+1
          to=(j-1)*repLength+k*valueLength
          col[from:to]=col[from:to]*probs[k]
        }
      }
      repeats=repeats*length(node$values)
    }

    row=genFromRow(col)
    return (getParentsValueFromRow(row,possibleEffVals))
  }
}  
getParameter2Row<-function(node,pIndex,pvals) {
  probs=rep(0,length(node$values))
  probs[1]=1
  row=NULL
  if (pIndex==0) {
    row=1
  } else if (pvals[pIndex]>1) {
    row=node$pindices[pIndex]+pvals[pIndex]-2
  }
  if (!is.null(row)) {
    probs=node$params[row,2:ncol(node$params)]
  }  
  return (probs)
}
