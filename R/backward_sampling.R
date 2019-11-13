#' Generate backward sample for node
#' 
#' S3 generic function for generate backward sample for node.
#' Alters sample, auxiliary samples and likelihood of sample.
#' @param node The node object.
#' @param s Sample object, including sample, auxiliary 
#' samples and likelihood (lh).
#' @return The updated sample object. 
#' @keywords internal
backwardsSample <- function (node,s,straightCDS=NULL,met="aux") {
  UseMethod('backwardsSample', node)
}
getColumnProbabilities<-function(col,rows,params) {
  out<-params[rows,col]
  for (i in 1:length(rows)) {
    out[i]=out[i]/sum(params[rows[i],])
  }
  return (out)
}

#' @keywords internal
backwardsSample.dirichlet<-function(node,s,straightCDS,met) {
  pvals<-s$sample[node$parents]
  val<-s$sample[node$index]
  if (!any(is.na(pvals))){
    row<-calculate.row(pvals,node$pvals)
    p<-node$params[row,val]/sum(node$params[row,])
    s$lh=s$lh*p
  }
  else {
    rows<-getPossibleRows(pvals,node$pvals)
    col<-getColumnProbabilities(val,rows,node$params)
    norm<-sum(col)
    s$lh=s$lh*norm
    col=col/norm
    row=genFromRow(col)    
    s$sample[node$parents]=getParentsValueFromRow(row,node$pvals)  
  }
  return (s)
}  
#' @keywords internal
backwardsSample.flag <- function (node,s,straightCDS,met) {
  val<-s$sample[node$index]
  pval<-s$sample[node$parents] # Only one parent since it is a flag node
  if (!is.na(pval)){
    p<-ifelse(identical(gen(node,pval),val),1,0)    
    s$lh=s$lh*p
  }
  else {
    if (val==2) {
      s$sample[node$parents]=node$params
    } else {
      s$sample[node$parents]=sample((1:node$pvals)[-node$params],1)
    }
  }  
  return(s)
}
#' @keywords internal
backwardsSample.logand <- function (node,s,straightCDS,met) {
  val<-s$sample[node$index]
  pvals<-s$sample[node$parents]
  if (!any(is.na(pvals))){
    p<-ifelse(identical(gen(node,pvals),val),1,0)    
    s$lh=s$lh*p
  }
  else {
    if (any(is.na(pvals))) {
      indices<-which(!is.na(pvals))
      match<-identical(node$params[indices],pvals[indices])
      if ( !match&&val==2 ) {
        # We still set values to appropriate, but lk now 0
        s$lh=0        
      }
    }
    if (val==1) {
      rows<-getPossibleRows(pvals,node$pvals)
      disallowed<-calculate.row(node$params,node$pvals)
      rows=setdiff(rows,disallowed)
      if (length(rows)==0) {
        s$lh=0                
        # We still set values to T, but lk now 0
        s$sample[node$parents]=node$params      
      } else {
        s$lh=s$lh*length(rows)
        row=sample(rows,1)       
        s$sample[node$parents]=getParentsValueFromRow(row,node$pvals)        
      }
    } else {
      s$sample[node$parents]=node$params      
    }
  }  
  return(s)
}
#' @keywords internal
backwardsSample.logprod<-function(node,s,straightCDS,met) {
  val<-s$sample[node$index]
  pvals<-s$sample[node$parents]
  if (!any(is.na(pvals))){
    p<-ifelse(identical(gen(node,pvals),val),1,0)    
    s$lh=s$lh*p
  }
  else {
    rows<-1:prod(pvals)
    row<-calculate.row(node$params,node$pvals)
    required<-getParentsValueFromRow(row,node$pvals)
    if (any(is.na(pvals))) {
      indices<-which(!is.na(pvals))
      p<-ifelse(identical(node$params[indices],required[indices]),1,0)
      s$lh=s$lh*p
    }
    s$sample[node$parents]=required
  }  
  return(s)  
}
#' @keywords internal
backwardsSample.flagprod<-function(node,s,straightCDS,met) {
  val<-s$sample[node$index]
  pvals<-s$sample[node$parents]
  if (!any(is.na(pvals))){
    p<-ifelse(identical(gen(node,pvals),val),1,0)    
    s$lh=s$lh*p
  }
  else {
    required<-getParentsValueFromRow(val,c(2,2))
    if (any(is.na(pvals))) {
      indices<-which(!is.na(pvals))
      p<-ifelse(identical(node$params[indices],required[indices]),1,0)
      s$lh=s$lh*p
    }
    s$sample[node$parents]=required
  }  
  return(s)    
}
#' @keywords internal
backwardsSample.logor<-function(node,s,straightCDS,met) {
  val<-s$sample[node$index]
  pvals<-s$sample[node$parents]
  if (!any(is.na(pvals))){
    p<-ifelse(identical(gen(node,pvals),val),1,0)    
    s$lh=s$lh*p
  }
  else {
    if (any(is.na(pvals))) {
      indices<-which(!is.na(pvals))
      match<-any(node$params[indices]==pvals[indices])
      if ( (match&&val==1) ) {
        # We still set values to appropriate, but lk now 0
        s$lh=0        
      }
    }
    rows<-getPossibleRows_LO(val,pvals,node$params,node$pvals)
    s$lh=s$lh*length(rows)
    row<-sample(rows,1)
    assigned<-getParentsValueFromRow(row,node$pvals)
    s$sample[node$parents]=assigned
  }  
  return(s)
}  
#' @keywords internal
backwardsSample.noisyor<-function(node,s,straightCDS,met) {
  if (identical(met,"aux")) {
    return (backwardsSample_aux.noisyor(node,s))
  } else {
    return (backwardsSample_nat.noisyor(node,s,straightCDS))
  }  
}
#' @keywords internal
backwardsSample_nat.noisyor<-function(node,s,straightCDS) {
  val<-s$sample[node$index]
  pvals<-s$sample[node$parents]
  if (!any(is.na(pvals))){
    row<-calculate.row(pvals,node$pvals)
    s$lh=s$lh*straightCDS[[node$index]][[val]][row]
  }
  else {
    rows<-getPossibleRows(pvals,node$pvals)
    col<-straightCDS[[node$index]][[val]]
    norm<-sum(col)
    s$lh=s$lh*norm
    col=col/norm
    row=genFromRow(col)    
    s$sample[node$parents]=getParentsValueFromRow(row,node$pvals)  
  }
  s$auxiliarySamples=
    createAuxiliarySamples(node,s$sample,s$auxiliarySamples,
                           s$sample[node$index],"MB")      
  return (s)
}
#' @keywords internal
backwardsSample_aux.noisyor<-function(node,s) {
  #   s<-list(
  #     sample=evidence,
  #     lh=1,
  #     auxiliarySamples=list()
  #   )  
  val<-s$sample[node$index]
  row=NULL
  # Get auxiliary samples
  # Sample effects row in params2
  #   There a (parents+1)^values rows in the params2 matrix.
  #   (Even if it is virtual when model is binary!)    
  effectiveParentsPossibleValues=rep(length(node$values),length(node$parents)+1)  
  slackDenom=sum(node$params[1,2:ncol(node$params)])
  if (val==1) {
    row=1
    s$lh=s$lh*node$params[1,2]/slackDenom
  } else {
    # Create column
    col=NULL
    if (length(node$values)==2) {
      col=rep(1,prod(effectiveParentsPossibleValues))
    } else {
      col=node$params2[,val]
    }

    # Adjust for known values
    col[1]=0
    effectiveParentValues<-getEffectiveParentValues(s$sample,node$parents)
    rows<-getPossibleRows(effectiveParentValues,effectiveParentsPossibleValues)
    col[-rows]=0
      
    # Adjust for slack probabilities 
    colLength=length(col)
    portionLength=colLength/length(node$values)
    for (i in 1:length(node$values)) {
      from<-((i-1)*portionLength+1)
      to<-(i*portionLength)
      col[from:to]=
        col[from:to]*
        node$params[1,i+1]/slackDenom
    }
      
    # Select row and update likelihood
    norm<-sum(col)
    s$lh=s$lh*norm
    col=col/norm
    row=genFromRow(col)
  }

  effects<-getParentsValueFromRow(row,effectiveParentsPossibleValues)  
  s$auxiliarySamples[[node$name]]=effects
  
  # Calculate slack probability 
  # params from 2 to skip effective parent index
#  prob=calculate.dprob(effects[1],node$params[1,2:ncol(node$params)])
#  s$lh=s$lh*prob  

  # Get parent samples or effective parent probabilities
  if (length(effects)>1) {
    for (i in 2:length(effects)) {
      if (is.na(s$sample[node$parents[i-1]])){
        rows=which(node$params[,1]==(i-1))
        col<-getColumnProbabilities(effects[i],rows,
                                    node$params[,2:ncol(node$params)])
        a<-ifelse(effects[i]==1,1,0)
        col<-append(a,col)
        norm<-sum(col)
        s$lh=s$lh*norm
        col=col/norm
        s$sample[node$parents[i-1]]=genFromRow(col)  
      }
      else if (s$sample[node$parents[i-1]]!=1) {
        # We find probability of effective parent      
        # Note that if real parent has value 1, then 
        # effective parent was known, and probability is 1, so do nothing.   
        rows=which(node$params[,1]==(i-1))
        prob=
          node$params[rows[s$sample[node$parents[i-1]]-1],effects[i]+1]/
          sum(node$params[rows[s$sample[node$parents[i-1]]-1],2:ncol(node$params)])
        s$lh=s$lh*prob
      }
    }      
  }
  return(s)
}
getEffectiveParentValues<-function(sample,parents){
  out<-rep(NA,length(parents)+1)
  known<-which(sample[parents]==1)
  out[known+1]=1
  return(out)
}
getPossibleRows<-function (parentValues,possibleParentValues){
  prows<-1:prod(possibleParentValues)        
  for (i in length(parentValues):1) {
    if (!is.na(parentValues[i])) {
      # Remove appropriate rows.
      valueSetSize=1
      if (i!=length(parentValues)) {
        valueSetSize=prod(possibleParentValues[(i+1):length(parentValues)])        
      }
      valuesSetSize=valueSetSize*possibleParentValues[i]
      f<-function(r) {
        # If row = 4, and col=3 from right, of 4 binary variables.
        # valueSetSize will be 4
        # valuesSetSize will be 8
        # 3 mod 8 = 3, + 1 = 4, / 4 = 1, ceiling = 1.
        ceiling((((r-1)%%(valuesSetSize))+1)/valueSetSize)==parentValues[i]
      }
      prows=prows[which(sapply(prows,f))]
    }
  }
  return (prows)  
}  
getParentValueFromRow <-function(row,parent,parentPossibleValues) {
  parents<-length(parentPossibleValues)
  if (parent!=parents) {
    for (i in parents:(parent+1)) {
      row<-ceiling(row/parentPossibleValues[i])
    }
  }
  return (((row-1)%%parentPossibleValues[parent])+1)
}
getParentsValueFromRow <-function(row,parentPossibleValues) {
  out<-rep(NA,length(parentPossibleValues))
  for (i in length(parentPossibleValues):1) {
    out[i]=row%%parentPossibleValues[i]
    if (out[i]==0) {
      out[i]=parentPossibleValues[i]
    }
    row<-ceiling(row/parentPossibleValues[i])
  }
  return (out)
}
getPossibleRows_LO<-function(
  val,
  parentValues,
  disjunctionValues,
  possibleParentValues
  ) {
  prows<-getPossibleRows(parentValues,possibleParentValues)
  totalrows<-1:prod(possibleParentValues)
  indices<-which(is.na(parentValues))
  for (i in indices) {
    unknown<-rep(NA,length(parentValues))
    unknown[i]=disjunctionValues[i]
    irows<-getPossibleRows(unknown,possibleParentValues)
    if (val==2) {
      irows=totalrows[-irows]
    }
    prows<-setdiff(prows,irows)
  }    
  return (prows)
}  
