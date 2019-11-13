#--------------------
# Primary Functionality: Add edge into node
#--------------------
#' Add an edge into/a parent to a node
#' 
#' Add an edge into/a parent to a node. Node must be Noisy-Or or Dirichlet.
#' @param net The network.
#' @param child The index of the node to add 
#' a parent to.
#' @param parent The index of the node to add as a parent.
#' @param params The new parameters for the child.
#' @param params The parameters to add to the child for the parent. 
#' @param params2 The disambiguation parameters to use for the child. This 
#' is used only if the child is a multinomial noisy-or node. If the child is 
#' multinomial noisy-or and this is NULL a weighted mutex disambiguation 
#' matrix is created.
#' @param cn A RODBC connection. Required if performing deep update.
#' @param deepUpdate Specifies whether a deep update should be performed.
#' @param dropImages Specifies if images should be dropped on a deepUpdate.
#' @return Updated network.
#' @export 
#--------------------
addEdge=function(
  net,
  child,
  parent,
  params,
  params2=NULL,
  cn=NULL,
  deepUpdate=F,
  dropImages=F
  ){
  # Validate connection present if deep update desired
  if (deepUpdate && is.null(cn)) stop ("Connection required for deep update.")

  # Sanity checks
  if (any(net$nodes[[child]]$parents==parent)) {
    stop("Parent already present.")
  } else if (parent==child) {
    stop("Node cannot be its own parent.")
  }
  
  # Perform shallow update and check that node is suitable
  if (class(net$nodes[[child]])=='dirichlet')
    net$nodes[[child]]=addEdgeDirichlet(net$nodes[[child]],parent,params)
  else if (class(net$nodes[[child]])=='noisyor')
    net=addEdgeNoisyOr(net,child,parent,params,params2)
  else 
    stop("Edges can only be added to Noisy-Or and Dirichlet nodes.")

  # Check network is still a DAG
  if (!isDAG(net)) stop("Proposed edge causes causal loop. It has not been added.")
    
  # Perform deep update if required
  if (deepUpdate) net=deepUpdateOnEdgeAdded(cn,net,parent,child,dropImages)
  
  # Return network
  return (net)
}
#--------------------

#--------------------
# Major Subcomponent: Add edge into a dirichlet node
#--------------------
#' Add an edge into/a parent to a dirichlet node
#' 
#' Add an edge into/a parent to a dirichlet node
#' @param net The network.
#' @param child The index of the dirichlet node to add 
#' a parent to.
#' @param parent The index of the node to add as a parent.
#' @param params The new parameters for the child. 
#' @return Updated network.
#' @keywords internal 
#--------------------
addEdgeDirichlet <- function (
  net,
  child,
  parent,
  params
  ){
  # Sanity checks on params matrix size  
  if (ncol(params)!=ncol(net$nodes[[child]]$params))
    stop("Invalid number of columns in parameter matrix.")
  if (nrow(params)!=length(net$nodes[[parent]]$values)*nrow(net$nodes[[child]]$params))
    stop("Invalid number of rows in parameter matrix.")
  
  # Update node
  net$nodes[[child]]$parents<-append(net$nodes[[child]]$parents,parent)
  net$nodes[[child]]$pvals<-append(net$nodes[[child]]$pvals,length(net$nodes[[parent]]$values))
  net$nodes[[child]]$params<-params
  
  # DAG check carried out in addEdge function
  
  # Return updated network
  return (net)
}
#--------------------
# Major Subcomponent: Add edge into a noisy-or node
#--------------------
#' Add an edge into/a parent to a noisy-or node
#' 
#' Add an edge into/a parent to a noisy-or node
#' @param net The network.
#' @param child The index of the noisy-or node to add a parent to.
#' @param parent The index of the node to add as a parent.
#' @param params The parameters to add to the child for the parent. This 
#' should be a matrix with as many rows as the parent node has 
#' non-zero values (I.e. one less than the number of values of the 
#' parent).  
#' @param params2 The disambiguation parameters to use for the child. This 
#' is used only if the child is multinomial. If the child is multinomial and this is NULL
#' a weighted mutex disambiguation matrix is created.
#' @keywords internal 
#--------------------
addEdgeNoisyOr <- function (
  net,
  child,
  parent,
  params,
  params2=NULL
  ){
  # Update node
  net$nodes[[child]]$parents<-c(net$nodes[[child]]$parents,parent)
  net$nodes[[child]]$pindices<-
    c(net$nodes[[child]]$pindices,nrow(net$nodes[[child]]$params)+1)
  net$nodes[[child]]$pvals<-
    c(net$nodes[[child]]$pvals,length(net$nodes[[parent]]$values))
  
  if (ncol(params)==ncol(net$nodes[[child]]$params)-1) {
    effIndex=net$nodes[[child]]$params[nrow(net$nodes[[child]]$params),1]+1
    params<-cbind(rep(effIndex,nrow(params)),params) 
  }  
  
  net$nodes[[child]]$params<-rbind(net$nodes[[child]]$params,params)
  rownames(net$nodes[[child]])<-NULL
  
  # Update parameters 2 for multinomial nodes
  if (length(net$nodes[[child]]$values)!=2) {
    if (is.null(params2)) {
      params2<-makeWeightedMutex(length(net$nodes[[child]]$parents),length(net$nodes[[child]]$values))
    }
    if (
      (ncol(params2)!=length(net$nodes[[child]]$values)) ||
        (nrow(params2)!=length(net$nodes[[child]]$values)^length(net$nodes[[child]]$pvals))    
    ) {
      stop('Invalid second parameter matrix.')    
    }
    net$nodes[[child]]$params2<-params2
  }  
  
  # DAG check carried out in addEdge function
  
  # Return network
  return (net)
}
#--------------------
# Major subcomponent: Perform deep update after adding an edge
#--------------------
#' Perform a deep update after adding an edge
#' 
#' Update the images and tables of a network after adding an edge.
#' Note that to be able to update the saved network images, the child must be 
#' a binary noisy or node. 
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param from The index of the parent node
#' @param to The index of the child node
#' @param dropImages Whether images should be dropped or updated. 
#' @param zeroIndexed Whether the from and to arguments are 0 or 1 indexed.
#' @return The updated network
#' @keywords internal
#--------------------
deepUpdateOnEdgeAdded<-function(
  cn,
  net,
  from,
  to,
  dropImages=F
){
  #   a.  Add edge in current network.
  #   b.  Check if child node is binary noisy-or.
  #     i.  If so, then in all images:
  #       1.	Add edge
  #         a.	Add parent to child’s parent list
  #         b.	Add number of values to child’s parent values list
  #         c.	Add rows to child parameters corresponding to new parent.
  #   ii.	If not then drop all images
  
  # Adjust for zero indexing
  from=from-1
  to=to-1
  
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,FALSE)    
  
  # Update nodes in network
  updateSerializedNetwork(cn,net,T)
  
  # Drop or update images
  updateImagesOnEdgeAdded(cn,net,dropImages,from,to)
  
  # No need to adjust data views or result tables 
  
  # Reset autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE)      
  
  # Return network
  return(net)
}
#--------------------

#--------------------
# Help function: Check that graph is a DAG
#--------------------
#' Check that graph is a dag
#' 
#' Check that graph is a dag. Note igraph::is.dag is buggy.
#' @param net The Bayesian network object
#' @return Boolean specifying if network a DAG
#' @keywords internal
#--------------------
isDAG <- function (net) {
  edges=matrix(getEdges(net),byrow=T,ncol=2)
  n=length(net$nodes)
    
  for (i in 1:n) {
    next.from<-unique(edges[which(edges[,1]==n),2])
    if (find.loop(edges,next.from,n)) {
      return (FALSE)
    }      
  }
  return (TRUE)
}
#--------------------
# Help function: Find loop in graph
#--------------------
#' Find a loop in graph
#' 
#' Find a loop in graph
#' @param edges the edge matrix
#' @param from the nodes we are looking for routes to the target from
#' @param target the node which a route to proves there is a loop 
#' @return Boolean specifying whether loop exists or not
#' @keywords internal
#--------------------
find.loop <- function (edges,from,target) {
  if (length(from)==0) {
    return (FALSE) # No loop found
  } else if (any(edges[which(edges[,1]%in%from),2]==target)) {
    return (TRUE) # Loop found
  } else {
    for (i in 1:length(from)) {
      next.from<-unique(edges[which(edges[,1]==from[i]),2])
      if (find.loop(edges,next.from,target)) {
        return (TRUE)
      }      
    }
    return (FALSE)
  }
}
#--------------------
# Help function: Update images after adding an edge
#--------------------
#' Update images after adding an edge
#' 
#' Update the images after adding an edge.
#' Note that to be able to update the saved network images, the child must be 
#' a binary noisy-or node. 
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param dropImages Whether images should be dropped or updated. 
#' @param to The index of the child node
#' @param from The index of the parent node
#' @keywords internal
#--------------------
updateImagesOnEdgeAdded=function(
  cn,
  net,
  dropImages,
  from,
  to
  ){
  if (dropImages || class(net$nodes[[to+1]])!="noisyor" || length(net$nodes[[to+1]]$values)!=2) {
    if (!dropImages) 
      warning ("Updating old images only works when removing edges into binary noisyor nodes.
               Images have been dropped.")
    SqlQuery(cn,paste("DELETE FROM",net$tableinfo$image_table,"WHERE 1"))  
  } 
  else {
    # Update Images
    # Adjust parent lists, and parent Values and parameters of children
    newPIndex=which(net$nodes[[to+1]]$parents==(from+1))
    addedParams=net$nodes[[to+1]]$params[which(net$nodes[[to+1]]$params[,1]==newPIndex),]
    str=paste("SELECT * FROM ",net$tableinfo$image_table," WHERE NodeIndex=",to,sep="")
    df=SqlQuery(cn,str)
    if (nrow(df)>0){
      for (i in 1:nrow(df)) {
        # Ajust parent values and parameters of children
        oldChild=unserializeNode(df[i,])# This should work. Col 1 is Time in images and Network in nodes.
        parents=c(oldChild$parents,from+1)
        parentValues=c(oldChild$pvals,length(net$nodes[[from]]$values))
        pindices=c(oldChild$pindices,nrow(oldChild$params)+1)
        newParents=encodeWSSS(parents-1)    
        newParentValues=encodeWSSS(parentValues)
        #   Replace children parameters with new parameters!
        params1=rbind(oldChild$params,addedParams)
        newParams1=encodeWSSS(params1)        
        params2Matrix=NULL
        if (length(pindices)>0) {
          params2Matrix=matrix(pindices-1,nrow=1)
        }
        newParams2=encodeWSSS(params2Matrix)
        newParams3=encodeWSSS(NULL)
        newDimensions=encodeDimensions(params1,params2Matrix,NULL)
        str=paste("UPDATE ",net$tableinfo$image_table," SET ",
                  "ParentValues=",newParentValues,
                  ",Params1=",newParams1,",Params2=",newParams2,",Params3=",newParams3,
                  ", parents=",newParents,",Dimensions=",newDimensions," WHERE Time='",df[i,1],
                  "' AND NodeIndex=",df[i,2],sep="")
        SqlQuery(cn,str)      
      }
    }
  }
}
#--------------------
